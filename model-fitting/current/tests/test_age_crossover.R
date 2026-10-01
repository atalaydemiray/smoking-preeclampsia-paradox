source("R/26_age_crossover.R")
checks<-0L;check<-function(x){stopifnot(isTRUE(x));checks<<-checks+1L}
close<-function(a,b,tol=1e-8)check(length(a)==length(b)&&max(c(0,abs(a-b)))<tol)
fails<-function(x)check(inherits(tryCatch(force(x),error=identity),"error"))
knots<-c(1,2,3);bounds<-c(0,4);nodes<-c(0,knots,4)
X<-.ac_design(nodes,knots,bounds)
beta_values<-function(y)as.numeric(solve(X,y))
V<-diag(1e-5,5)
linear<-beta_values(nodes-2.5)
a<-analyze_age_crossover(linear,V,knots,bounds)
check(a$root_summary$n_isolated_roots==1L);close(a$roots$age,2.5)
check(a$root_summary$unique_regular_interior_root);check(!a$root_summary$has_zero_intervals)
close(a$roots$slope,1);check(a$roots$regular_delta);check(is.finite(a$roots$se_delta))
xx<-.ac_design(2.5,knots,bounds);close(a$roots$se_delta,sqrt(as.numeric(xx%*%V%*%t(xx))))
close(a$roots$delta_lower,2.5-qnorm(.975)*a$roots$se_delta)
close(a$roots$delta_upper,2.5+qnorm(.975)*a$roots$se_delta)
check(a$simultaneous$both_signs_demonstrated);check(a$pointwise$both_signs_demonstrated)
check(a$omnibus$df==4L);close(a$omnibus$statistic,as.numeric(crossprod(linear[-1],solve(V[-1,-1],linear[-1]))))
close(a$simultaneous$critical,sqrt(qchisq(.95,5)));close(a$pointwise$critical,qnorm(.975))
close(evaluate_age_crossover(a,2.5)$h,0);close(evaluate_age_crossover(a,c(0,1,2,3,4))$slope,rep(1,5))
# No roots, all-zero continuum, boundary and low-slope nonregular roots.
none<-analyze_age_crossover(c(2,0,0,0,0),V,knots,bounds)
check(nrow(none$roots)==0);check(!none$root_summary$has_zero_intervals)
check(nrow(none$pointwise$confidence_set)==0);check(none$simultaneous$has_strict_positive)
check(!none$simultaneous$has_strict_negative&&!none$simultaneous$both_signs_demonstrated)
zero<-analyze_age_crossover(rep(0,5),V,knots,bounds)
check(nrow(zero$roots)==0);check(zero$root_summary$has_zero_intervals)
check(nrow(zero$zero_intervals)==1);close(unlist(zero$zero_intervals[c("lower","upper")]),bounds)
check(nrow(zero$pointwise$confidence_set)==1);close(unlist(zero$pointwise$confidence_set[c("lower","upper")]),bounds)
check(zero$omnibus$statistic==0&&zero$omnibus$p_value==1)
check(!zero$simultaneous$both_signs_demonstrated);check(!zero$root_summary$unique_regular_interior_root)
edge<-analyze_age_crossover(beta_values(nodes),V,knots,bounds)
close(edge$roots$age,0);check(edge$roots$boundary);check(!edge$roots$regular_delta)
check(is.na(edge$roots$se_delta)&&is.na(edge$roots$delta_lower)&&is.na(edge$roots$delta_upper))
right<-analyze_age_crossover(beta_values(nodes-4),V,knots,bounds);close(right$roots$age,4);check(right$roots$boundary)
check(edge$pointwise$confidence_set$lower==0&&edge$pointwise$confidence_set$lower_closed)
check(right$pointwise$confidence_set$upper==4&&right$pointwise$confidence_set$upper_closed)
check(edge$pointwise$confidence_set$upper<4);check(right$pointwise$confidence_set$lower>0)
weak<-analyze_age_crossover(linear*1e-12,V,knots,bounds)
close(weak$roots$age,2.5);check(!weak$roots$regular_delta);check(is.na(weak$roots$se_delta))
check(grepl("flat_or_weak",weak$roots$status));check(!weak$root_summary$unique_regular_interior_root)
# Multiple roots: all real roots retained, not just a preferred branch.
osc<-analyze_age_crossover(beta_values(c(-1,1,-1,1,-1)),V,knots,bounds)
check(nrow(osc$roots)==4);check(all(osc$roots$age>c(0,1,2,3)&osc$roots$age<c(1,2,3,4)))
check(all(osc$roots$regular_delta));check(!osc$root_summary$unique_regular_interior_root)
check(nrow(osc$pointwise$confidence_set)==4);check(nrow(osc$simultaneous$confidence_set)==4)
check(osc$simultaneous$both_signs_demonstrated);check(all(osc$pointwise$confidence_set$lower_closed))
check(all(osc$pointwise$confidence_set$upper_closed))
check(all(!osc$pointwise$sign_regions$lower_closed[osc$pointwise$sign_regions$lower>0&osc$pointwise$sign_regions$classification!="zero_compatible"]))
# Tangent at knot; no finite implicit-delta interval is manufactured.
tangent<-analyze_age_crossover(beta_values(c(4,1,0,1,4)),V,knots,bounds)
check(nrow(tangent$roots)==1);close(tangent$roots$age,2)
check(abs(tangent$roots$slope)<1e-8);check(!tangent$roots$regular_delta)
check(is.na(tangent$roots$se_delta));check(!tangent$root_summary$unique_regular_interior_root)
# Odd flat crossing is a stationary/weak contact, never mislabeled a tangency.
P2<-a$pieces[[3]]$basis_coefficients
hermite<-rbind(X[1,],X[5,],X[3,],P2[2,],2*P2[3,])
odd_beta<-as.numeric(solve(hermite,c(-1,1,0,0,0)))
odd<-analyze_age_crossover(odd_beta,V,knots,bounds)
close(odd$roots$age,2);check(odd$roots$kind=="stationary_or_weak_contact")
check(!odd$roots$regular_delta);check(odd$diagnostics$point_estimate_has_both_signs)
# Interior even-multiplicity root on a cubic piece with generic knot count.
kk<-c(1,3);bb<-c(0,4);xx<-.ac_design(c(0,1,3,4),kk,bb)
bt<-as.numeric(solve(xx,c(4,1,1,4)));bt[1]<-bt[1]-as.numeric(.ac_design(2,kk,bb)%*%bt)
tt<-analyze_age_crossover(bt,diag(1e-5,4),kk,bb)
check(nrow(tt$roots)==1);close(tt$roots$age,2);check(!tt$roots$regular_delta);check(tt$omnibus$df==3)
# An entire zero piece is not reported as finitely many point roots.
P<-a$pieces[[1]]$basis_coefficients;ss<-svd(P,nu=4,nv=5);rank<-sum(ss$d>max(ss$d)*1e-10)
flat_beta<-ss$v[,rank+1L];flat<-analyze_age_crossover(flat_beta,V,knots,bounds)
check(flat$root_summary$has_zero_intervals);check(any(flat$zero_intervals$lower<=0&flat$zero_intervals$upper>=1))
check(!any(flat$roots$age>=0&flat$roots$age<=1));check(!flat$root_summary$unique_regular_interior_root)
# Polynomial isolation unit tests include degree6, tangent, close roots, all-zero.
tol<-a$diagnostics$tolerances
coef<-1;for(r in c(.15,.15,.35,.55,.8,.95))coef<-.ac_multiply(coef,c(-r,1))
pr<-.ac_poly_roots(coef,tol);close(pr$roots,c(.15,.35,.55,.8,.95),2e-7)
check(length(pr$contacts)>=1);check(!pr$zero_piece)
close(.ac_poly_roots(c(.25-1e-12,-1,1),tol)$roots,c(.5-1e-6,.5+1e-6),1e-8)
close(.ac_poly_roots(c(0,1,-1),tol)$roots,c(0,1));check(.ac_poly_roots(rep(0,7),tol)$zero_piece)
check(length(.ac_poly_roots(c(1,0,1),tol)$roots)==0)
# Confidence set can be nonempty without any point root; no existence assertion.
broad_uncertainty<-analyze_age_crossover(c(.01,0,0,0,0),diag(1,5),knots,bounds)
check(nrow(broad_uncertainty$roots)==0);check(nrow(broad_uncertainty$simultaneous$confidence_set)==1)
check(!broad_uncertainty$simultaneous$root_existence_established_by_nonempty_set)
check(grepl("fixed prespecified",a$pointwise$inference_label));check(grepl("outer confidence SET",a$simultaneous$inference_label))
check(!a$pointwise$both_signs_joint_level_controlled);check(a$simultaneous$both_signs_joint_level_controlled)
# Degree6 inversion contact: a singleton zero-compatible set is retained.
Vnodes<-unname(solve(X)%*%t(solve(X)));cval<-qnorm(.975)
single<-analyze_age_crossover(cval*beta_values(c(5,2,1,2,5)),Vnodes,knots,bounds)
check(nrow(single$pointwise$confidence_set)==1);close(single$pointwise$confidence_set$lower,2,1e-7)
close(single$pointwise$confidence_set$upper,2,1e-7);check(single$pointwise$confidence_set$kind=="singleton")
# Low-level band inversion can cover an entire zero polynomial segment.
pc<-list(list(lower=0,upper=1,h_coefficients=c(1,0,0,0),variance_coefficients=c(1,rep(0,6))))
entire<-.ac_band(pc,1,tol,1e-10,"unit test")
close(unlist(entire$confidence_set[c("lower","upper")]),c(0,1));check(entire$identically_zero_band_boundary_pieces==1)
# Continuous polynomial results agree with direct basis evaluations, while grids
# are cross-checks only and never the root-finding guarantee.
inside<-function(sets,x) vapply(x,function(t)any((t>sets$lower|(sets$lower_closed&t==sets$lower))&
  (t<sets$upper|(sets$upper_closed&t==sets$upper))),TRUE)
for(z in list(a,osc,tangent,tt,zero,none,broad_uncertainty)) {
  grid<-seq(z$bounds[1],z$bounds[2],length.out=1001);ev<-evaluate_age_crossover(z,grid)
  direct<-as.numeric(.ac_design(grid,z$knots,z$bounds)%*%z$beta)
  close(ev$h,direct,1e-12);check(all(is.finite(ev$SE)&ev$SE>0))
  for(type in c("pointwise","simultaneous")) {
    c<-z[[type]]$critical;q<-ev$h^2-c^2*ev$SE^2;notclose<-abs(q)>1e-9*max(1,max(abs(q)))
    check(identical(inside(z[[type]]$confidence_set,grid)[notclose],(q<=0)[notclose]))
  }
  check(all(!inside(z$pointwise$confidence_set,grid)|inside(z$simultaneous$confidence_set,grid)))
}
# General k=2 no-interior-knots case and named order / covariance refusal gates.
ll<-analyze_age_crossover(c(-.5,1),diag(.01,2),numeric(),c(0,4));check(ll$omnibus$df==1)
bn<-setNames(linear,c("A",paste0("A_ns",1:4)));vn<-V;dimnames(vn)<-list(names(bn),names(bn))
named<-analyze_age_crossover(bn,vn,knots,bounds);close(named$roots$age,a$roots$age)
bad<-vn;colnames(bad)<-rev(colnames(bad));fails(analyze_age_crossover(bn,bad,knots,bounds))
fails(analyze_age_crossover(bn,V,knots,bounds));fails(analyze_age_crossover(linear,vn,knots,bounds))
bad<-V;bad[1,1]<- -1;fails(analyze_age_crossover(linear,bad,knots,bounds))
bad<-V;bad[1,2]<-1;fails(analyze_age_crossover(linear,bad,knots,bounds))
bad<-V;bad[1,1]<-1e-25;fails(analyze_age_crossover(linear,bad,knots,bounds))
fails(analyze_age_crossover(linear,diag(4),knots,bounds));fails(analyze_age_crossover(c(NA,linear[-1]),V,knots,bounds))
fails(analyze_age_crossover(linear,V,c(1,1,3),bounds));fails(analyze_age_crossover(linear,V,c(0,2,3),bounds))
fails(analyze_age_crossover(linear,V,knots,rev(bounds)));fails(analyze_age_crossover(linear,V,knots,bounds,level=1))
fails(analyze_age_crossover(linear,V,knots,bounds,tolerances=list(unknown=1)))
fails(analyze_age_crossover(linear,V,knots,bounds,tolerances=list(root=0)))
fails(evaluate_age_crossover(a,c(-1,1)));fails(evaluate_age_crossover(a,c(1,NA)))
# Independent implicit derivative check by coefficient perturbation, no fitting.
epsilon<-1e-5;der<-numeric(5)
for(j in 1:5){plus<-minus<-linear;plus[j]<-plus[j]+epsilon;minus[j]<-minus[j]-epsilon
  rplus<-analyze_age_crossover(plus,V,knots,bounds)$roots$age
  rminus<-analyze_age_crossover(minus,V,knots,bounds)$roots$age
  der[j]<-(rplus-rminus)/(2*epsilon)}
close(der,-as.numeric(xx<-.ac_design(2.5,knots,bounds)),1e-7)
close(as.numeric(der%*%V%*%der),a$roots$se_delta^2,1e-10)
# Scale invariance and an untruncated, wide secondary local-delta interval.
scaled<-analyze_age_crossover(linear*1e-12,V*1e-24,knots,bounds)
close(scaled$roots$age,a$roots$age);close(scaled$roots$se_delta,a$roots$se_delta)
check(scaled$roots$regular_delta);close(scaled$pointwise$confidence_set$lower,a$pointwise$confidence_set$lower)
close(scaled$pointwise$confidence_set$upper,a$pointwise$confidence_set$upper)
wide<-analyze_age_crossover(linear,diag(100,5),knots,bounds)
check(wide$roots$delta_lower<0&&wide$roots$delta_upper>4)
cat("PASS:",checks,"age crossover polynomial/inference-contract checks; synthetic only\n")
