# Pure fixed-natural-spline crossover engine. No clinical data, I/O or fitting.
# Polynomial isolation is to explicit floating-point tolerances, not a formal
# interval-arithmetic certificate. Flat contacts and zero pieces stay explicit.

.ac_eval_poly <- function(coef,x) {
  out<-rep(0,length(x));for(j in rev(seq_along(coef)))out<-out*x+coef[j];out
}
.ac_derivative <- function(coef)if(length(coef)<2L)0 else coef[-1L]*seq_len(length(coef)-1L)
.ac_multiply <- function(a,b) {
  z<-numeric(length(a)+length(b)-1L)
  for(i in seq_along(a))for(j in seq_along(b))z[i+j-1L]<-z[i+j-1L]+a[i]*b[j]
  z
}
.ac_unique <- function(x,tol) {
  if(!length(x))return(numeric());x<-sort(x)
  x[c(TRUE,diff(x)>tol)]
}
.ac_empty_components <- function()data.frame(lower=numeric(),upper=numeric(),
  lower_closed=logical(),upper_closed=logical(),kind=character(),stringsAsFactors=FALSE)
.ac_merge_components <- function(d,tol) {
  if(!nrow(d))return(.ac_empty_components())
  d<-d[order(d$lower,d$upper),,drop=FALSE];out<-list();current<-d[1,,drop=FALSE]
  if(nrow(d)>1L)for(i in 2:nrow(d)) {
    nextrow<-d[i,,drop=FALSE]
    touch<-abs(nextrow$lower-current$upper)<=tol
    connected<-nextrow$lower<current$upper-tol || (touch&&(current$upper_closed||nextrow$lower_closed))
    if(connected) {
      if(nextrow$upper>current$upper+tol){current$upper<-nextrow$upper;current$upper_closed<-nextrow$upper_closed}
      else if(abs(nextrow$upper-current$upper)<=tol)current$upper_closed<-current$upper_closed||nextrow$upper_closed
      if(abs(nextrow$lower-current$lower)<=tol)current$lower_closed<-current$lower_closed||nextrow$lower_closed
    } else {out[[length(out)+1L]]<-current;current<-nextrow}
  }
  out[[length(out)+1L]]<-current;ans<-do.call(rbind,out);rownames(ans)<-NULL
  ans$kind<-ifelse(abs(ans$upper-ans$lower)<=tol,"singleton","interval");ans
}

# Rolle/monotonic-interval recursion enumerates all polynomial zeros: derivative
# roots partition monotone pieces; sign changes are bracketed, and stationary
# zeros retain even-multiplicity contacts. Dense grids are verification only.
.ac_poly_roots <- function(coef,tol,reference_scale=NULL) {
  if(!is.numeric(coef)||!length(coef)||any(!is.finite(coef))||length(coef)>7L)stop("Finite degree<=6 polynomial required")
  scale<-max(abs(coef));reference_scale<-if(is.null(reference_scale))scale else reference_scale
  if(scale==0 || (reference_scale>0&&scale<=tol$piece_zero*reference_scale))
    return(list(roots=numeric(),zero_piece=TRUE,contacts=numeric(),max_residual=0))
  c<-coef/scale
  while(length(c)>1L&&abs(tail(c,1))<=tol$trim)c<-head(c,-1L)
  degree<-length(c)-1L
  if(!degree)return(list(roots=numeric(),zero_piece=FALSE,contacts=numeric(),max_residual=0))
  if(degree==1L) {
    r<- -c[1]/c[2];r<-r[is.finite(r)&r>=-tol$root&r<=1+tol$root]
    r[abs(r)<=tol$root]<-0;r[abs(r-1)<=tol$root]<-1
    return(list(roots=r,zero_piece=FALSE,contacts=numeric(),max_residual=if(length(r))max(abs(.ac_eval_poly(coef,r)))else 0))
  }
  derivative<-.ac_poly_roots(.ac_derivative(c),tol)
  cuts<-.ac_unique(c(0,derivative$roots,1),tol$root)
  # A user residual tolerance must not collapse two resolved close roots into
  # one tangent. Only an evaluation-roundoff-size contact is treated as zero.
  contact_tol<-min(tol$value,64*.Machine$double.eps*sum(abs(c)))
  values<-.ac_eval_poly(c,cuts);near<-abs(values)<=contact_tol
  roots<-cuts[near];contacts<-cuts[near&cuts>tol$root&cuts<1-tol$root]
  for(i in seq_len(length(cuts)-1L)) {
    if(!near[i]&&!near[i+1L]&&sign(values[i])!=sign(values[i+1L])) {
      r<-stats::uniroot(function(x).ac_eval_poly(c,x),cuts[c(i,i+1L)],tol=tol$root/10)$root
      roots<-c(roots,r)
    }
  }
  roots<-.ac_unique(roots,tol$root)
  residual<-if(length(roots))max(abs(.ac_eval_poly(coef,roots)))else 0
  if(residual>10*tol$value*scale)stop("Polynomial root residual exceeds declared tolerance")
  list(roots=roots,zero_piece=FALSE,contacts=contacts,max_residual=residual)
}

.ac_validate <- function(beta,V,knots,bounds,level,tolerances) {
  defaults<-list(root=1e-12,value=2e-11,trim=5e-14,piece_zero=2e-12,
    reconstruction=2e-10,slope=1e-8,symmetry=1e-12,min_rcond=1e-13)
  if(!is.list(tolerances)||any(!names(tolerances)%in%names(defaults))||
    (length(tolerances)&&is.null(names(tolerances))))stop("Named known tolerances required")
  tol<-utils::modifyList(defaults,tolerances)
  if(any(!vapply(tol,function(x)is.numeric(x)&&length(x)==1L&&is.finite(x)&&x>0,TRUE)))stop("Positive finite scalar tolerances required")
  if(!is.numeric(knots)||any(!is.finite(knots))||is.unsorted(knots,strictly=TRUE)||
    !is.numeric(bounds)||length(bounds)!=2L||any(!is.finite(bounds))||bounds[1]>=bounds[2]||
    any(knots<=bounds[1]|knots>=bounds[2]))stop("Explicit increasing interior knots and two boundaries required")
  k<-length(knots)+2L
  if(!is.numeric(beta)||is.matrix(beta)||length(beta)!=k||any(!is.finite(beta)))stop("Beta must contain exposure main then fixed ns terms in order")
  if(!is.matrix(V)||!is.numeric(V)||!identical(dim(V),c(k,k))||any(!is.finite(V)))stop("Matching finite numeric covariance matrix required")
  if(!is.null(names(beta))||!is.null(rownames(V))||!is.null(colnames(V))) {
    if(is.null(names(beta))||anyDuplicated(names(beta))||anyNA(names(beta))||any(!nzchar(names(beta)))||
      !identical(names(beta),rownames(V))||!identical(names(beta),colnames(V)))stop("Named beta/covariance order must agree exactly")
  }
  scale<-max(abs(V));if(scale==0||max(abs(V-t(V)))>tol$symmetry*scale)stop("Symmetric positive-definite covariance required")
  # Refuse materially asymmetric matrices; averaging accepted roundoff is recorded.
  asymmetry<-max(abs(V-t(V)));V<-(V+t(V))/2
  ch<-tryCatch(chol(V),error=identity);if(inherits(ch,"error"))stop("Covariance is not positive definite")
  rc<-rcond(V);if(!is.finite(rc)||rc<tol$min_rcond)stop("Covariance is numerically ill-conditioned; no regularization permitted")
  if(!is.numeric(level)||length(level)!=1L||!is.finite(level)||level<=0||level>=1)stop("Confidence level must lie strictly between0 and1")
  list(beta=beta,V=V,k=k,tol=tol,rcond=rc,accepted_asymmetry=asymmetry)
}
.ac_design <- function(age,knots,bounds)cbind(1,splines::ns(age,knots=knots,Boundary.knots=bounds,intercept=FALSE))
.ac_pieces <- function(beta,V,knots,bounds,tol) {
  cuts<-c(bounds[1],knots,bounds[2]);nodes<-c(0,.25,.75,1);vand<-outer(nodes,0:3,`^`)
  pieces<-vector("list",length(cuts)-1L);max_basis<-max_h<-max_v<-0
  for(j in seq_along(pieces)) {
    lower<-cuts[j];upper<-cuts[j+1L];width<-upper-lower
    P<-solve(vand,.ac_design(lower+width*nodes,knots,bounds))
    h<-as.numeric(P%*%beta);v<-numeric(7)
    for(a in 1:4)for(b in 1:4)v[a+b-1L]<-v[a+b-1L]+as.numeric(P[a,,drop=FALSE]%*%V%*%P[b,])
    # Non-node deterministic verification; not used to locate roots.
    u<-c(.071,.143,.271,.419,.563,.697,.853,.941)
    X<-.ac_design(lower+width*u,knots,bounds);H<-as.numeric(X%*%beta);VV<-rowSums((X%*%V)*X)
    er_basis<-max(abs(outer(u,0:3,`^`)%*%P-X))
    er_h<-max(abs(.ac_eval_poly(h,u)-H));er_v<-max(abs(.ac_eval_poly(v,u)-VV))
    if(any(!is.finite(c(P,h,v)))||any(VV<=0)||
      er_basis>tol$reconstruction*max(1,max(abs(X)))||
      er_h>tol$reconstruction*max(sum(abs(beta)),max(abs(h)),.Machine$double.xmin)||
      er_v>tol$reconstruction*max(max(abs(v)),.Machine$double.xmin))stop("Piecewise reconstruction/variance sanity check failed")
    max_basis<-max(max_basis,er_basis);max_h<-max(max_h,er_h);max_v<-max(max_v,er_v)
    pieces[[j]]<-list(lower=lower,upper=upper,basis_coefficients=P,h_coefficients=h,variance_coefficients=v)
  }
  list(pieces=pieces,errors=c(basis=max_basis,h=max_h,variance=max_v))
}

.ac_band <- function(pieces,critical,tol,age_tol,label,simultaneous=FALSE) {
  qcoef<-lapply(pieces,function(p).ac_multiply(p$h_coefficients,p$h_coefficients)-critical^2*p$variance_coefficients)
  ref<-max(abs(unlist(qcoef)));atoms<-list();root_records<-list();zero_count<-0L
  for(j in seq_along(pieces)) {
    p<-pieces[[j]];q<-qcoef[[j]]
    iso<-.ac_poly_roots(q,tol,ref);if(iso$zero_piece)zero_count<-zero_count+1L
    cuts<-.ac_unique(c(0,iso$roots,1),tol$root);width<-p$upper-p$lower
    classify<-function(u,is_root=FALSE) {
      if(iso$zero_piece||is_root||.ac_eval_poly(q,u)<=0)return("zero_compatible")
      if(.ac_eval_poly(p$h_coefficients,u)>0)"strict_positive"else"strict_negative"
    }
    add<-function(lo,hi,lc,uc,cl) {
      atoms[[length(atoms)+1L]]<<-data.frame(lower=p$lower+width*lo,upper=p$lower+width*hi,
        lower_closed=lc,upper_closed=uc,kind=if(lo==hi)"singleton"else"interval",classification=cl)
    }
    for(i in seq_along(cuts))add(cuts[i],cuts[i],TRUE,TRUE,
      classify(cuts[i],any(abs(cuts[i]-iso$roots)<=tol$root)))
    for(i in seq_len(length(cuts)-1L))add(cuts[i],cuts[i+1L],FALSE,FALSE,classify(mean(cuts[c(i,i+1L)])))
    root_records[[j]]<-list(piece=j,polynomial=q,normalized_roots=iso$roots,
      stationary_contacts=iso$contacts,zero_piece=iso$zero_piece,max_residual=iso$max_residual)
  }
  d<-do.call(rbind,atoms);sets<-list();regions<-list()
  for(cl in c("strict_negative","zero_compatible","strict_positive")) {
    dd<-.ac_merge_components(d[d$classification==cl,setdiff(names(d),"classification"),drop=FALSE],age_tol)
    sets[[cl]]<-dd;if(nrow(dd)){dd$classification<-cl;regions[[cl]]<-dd}
  }
  regions<-do.call(rbind,regions);rownames(regions)<-NULL;regions<-regions[order(regions$lower,regions$upper),,drop=FALSE]
  positive<-nrow(sets$strict_positive)>0;negative<-nrow(sets$strict_negative)>0
  list(critical=critical,confidence_set=sets$zero_compatible,sign_regions=regions,
    has_strict_positive=positive,has_strict_negative=negative,both_signs_demonstrated=positive&&negative,
    both_signs_joint_level_controlled=simultaneous&&positive&&negative,
    qualitative_sign_label=if(simultaneous)
      "Strict positive and negative simultaneous-band regions demonstrate qualitative reversal by continuity at the joint asymptotic level" else
      "Positive and negative pointwise-band regions are not a joint level-controlled reversal test",
    inference_label=label,simultaneous=simultaneous,root_existence_established_by_nonempty_set=FALSE,
    polynomial_inversion=root_records,identically_zero_band_boundary_pieces=zero_count)
}

evaluate_age_crossover <- function(object,age) {
  if(!inherits(object,"age_crossover_analysis")||!is.numeric(age)||any(!is.finite(age))||
    any(age<object$bounds[1]|age>object$bounds[2]))stop("Crossover object and finite in-bound ages required")
  X<-.ac_design(age,object$knots,object$bounds);h<-as.numeric(X%*%object$beta)
  vv<-rowSums((X%*%object$V)*X);if(any(vv<=0)||any(!is.finite(vv)))stop("Nonpositive/nonfinite contrast variance")
  slopes<-vapply(age,function(a){
    j<-findInterval(a,c(object$bounds[1],object$knots,object$bounds[2]),all.inside=TRUE)
    p<-object$pieces[[j]];.ac_eval_poly(.ac_derivative(p$h_coefficients),(a-p$lower)/(p$upper-p$lower))/(p$upper-p$lower)
  },0)
  se<-sqrt(vv);cp<-object$pointwise$critical;cs<-object$simultaneous$critical
  data.frame(age=age,h=h,SE=se,slope=slopes,pointwise_lower=h-cp*se,pointwise_upper=h+cp*se,
    simultaneous_lower=h-cs*se,simultaneous_upper=h+cs*se)
}

analyze_age_crossover <- function(beta,V,knots,bounds=c(15,44),level=.95,tolerances=list()) {
  valid<-.ac_validate(beta,V,knots,bounds,level,tolerances);V<-valid$V;tol<-valid$tol
  pp<-.ac_pieces(beta,V,knots,bounds,tol);pieces<-pp$pieces
  age_tol<-tol$root*diff(bounds);ref<-max(abs(unlist(lapply(pieces,`[[`,"h_coefficients"))))
  rr<-numeric();zero<-list();contacts<-numeric();extrema<-numeric();root_residual<-0
  for(j in seq_along(pieces)) {
    p<-pieces[[j]];width<-p$upper-p$lower;iso<-.ac_poly_roots(p$h_coefficients,tol,ref)
    pieces[[j]]$root_isolation<-iso
    if(iso$zero_piece)zero[[length(zero)+1L]]<-data.frame(lower=p$lower,upper=p$upper,lower_closed=TRUE,upper_closed=TRUE,kind="interval")
    else rr<-c(rr,p$lower+width*iso$roots)
    contacts<-c(contacts,p$lower+width*iso$contacts);root_residual<-max(root_residual,iso$max_residual)
    dr<-.ac_poly_roots(.ac_derivative(p$h_coefficients),tol)
    extrema<-c(extrema,.ac_eval_poly(p$h_coefficients,c(0,dr$roots,1)))
  }
  zero<-if(length(zero)).ac_merge_components(do.call(rbind,zero),age_tol)else .ac_empty_components()
  rr<-.ac_unique(rr,age_tol)
  # Snap shared knots/boundaries and remove isolated labels inside a zero continuum.
  cuts<-c(bounds[1],knots,bounds[2]);rr<-vapply(rr,function(r){j<-which.min(abs(cuts-r));if(abs(cuts[j]-r)<=age_tol)cuts[j]else r},0)
  if(nrow(zero))rr<-rr[!vapply(rr,function(r)any(r>=zero$lower-age_tol&r<=zero$upper+age_tol),TRUE)]
  pointwise<-.ac_band(pieces,stats::qnorm((1+level)/2),tol,age_tol,
    "Asymptotic pointwise-band inversion confidence SET for a fixed prespecified true root; not joint coverage of all roots or a selected-branch CI")
  simultaneous<-.ac_band(pieces,sqrt(stats::qchisq(level,df=valid$k)),tol,age_tol,
    "Asymptotic conservative continuous ellipsoid band; inversion is an outer confidence SET for the entire true zero set, not proof any root exists",TRUE)
  interaction<-beta[-1L];W<-as.numeric(crossprod(interaction,solve(V[-1L,-1L,drop=FALSE],interaction)))
  obj<-list(beta=beta,V=V,knots=knots,bounds=bounds,level=level,pieces=pieces,pointwise=pointwise,simultaneous=simultaneous,
    zero_intervals=zero,omnibus=list(statistic=W,df=valid$k-1L,p_value=stats::pchisq(W,valid$k-1L,lower.tail=FALSE),
      null="All exposure-by-age fixed natural-spline coefficients equal zero; asymptotic Wald test"),
    diagnostics=list(tolerances=tol,covariance_rcond=valid$rcond,accepted_covariance_asymmetry=valid$accepted_asymmetry,
      reconstruction_max_absolute_error=pp$errors,maximum_h_root_residual=root_residual,
      stationary_contacts_within_tolerance=contacts,
      method="Cubic fixed-basis reconstruction; Rolle/derivative-recursive root isolation; degree6 band inversion",
      floating_point_caveat="Polynomial isolation uses explicit tolerances; stationary contacts and near-zero pieces are numerically qualified, not formal interval-arithmetic certification",
      point_estimate_minimum=min(extrema),point_estimate_maximum=max(extrema),point_estimate_has_both_signs=min(extrema)<0&&max(extrema)>0))
  class(obj)<-"age_crossover_analysis"
  # Slope screening is relative to coefficient/uncertainty scale per age unit,
  # so jointly rescaling beta and covariance does not change regularity.
  slope_threshold<-tol$slope*max(max(abs(beta)),sqrt(max(diag(V))))/diff(bounds)
  obj$diagnostics$slope_threshold<-slope_threshold
  if(length(rr)) {
    ev<-evaluate_age_crossover(obj,rr);boundary<-abs(rr-bounds[1])<=age_tol|abs(rr-bounds[2])<=age_tol
    direct_residual<-max(abs(ev$h))
    if(direct_residual>10*tol$reconstruction*max(sum(abs(beta)),.Machine$double.xmin))stop("Direct basis root residual failed")
    obj$diagnostics$maximum_direct_basis_root_residual<-direct_residual
    regular<-abs(ev$slope)>slope_threshold & !boundary
    se<-ifelse(regular,ev$SE/abs(ev$slope),NA_real_)
    status<-ifelse(boundary,"boundary_root_local_interior_delta_not_reported",
      ifelse(regular,"regular_local_implicit_delta_secondary","flat_or_weak_slope_no_finite_delta_interval"))
    roots<-data.frame(age=rr,slope=ev$slope,kind=ifelse(boundary,"boundary",ifelse(abs(ev$slope)<=slope_threshold,"stationary_or_weak_contact","crossing")),
      boundary=boundary,regular_delta=regular,se_delta=se,delta_lower=rr-pointwise$critical*se,
      delta_upper=rr+pointwise$critical*se,status=status,stringsAsFactors=FALSE)
  } else {
    roots<-data.frame(age=numeric(),slope=numeric(),kind=character(),boundary=logical(),regular_delta=logical(),
      se_delta=numeric(),delta_lower=numeric(),delta_upper=numeric(),status=character())
    obj$diagnostics$maximum_direct_basis_root_residual<-0
  }
  obj$roots<-roots
  obj$root_summary<-list(n_isolated_roots=nrow(roots),has_zero_intervals=nrow(zero)>0,
    unique_regular_interior_root=nrow(roots)==1L&&!nrow(zero)&&all(roots$regular_delta),
    delta_intervals_secondary_local_not_simultaneous=TRUE,delta_intervals_not_clipped_to_analysis_bounds=TRUE,
    nonempty_inversion_set_does_not_prove_root_existence=TRUE)
  obj
}
