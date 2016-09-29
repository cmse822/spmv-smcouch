# Sparse Matrix-vector Multiply

1. Consider the sparse matrix-vector multiply code provided, `spmv.f90`. This codes implements the "Compressed Row Storage" format for a sparse matrix (see section 3.6 of HW). Per kernel inner loop iteration, how many FLOPs are performed?
2. Copy the code to a new file named `spmv_2.f90`. You may feel free to re-write the code instead in `C` or `C++`, if you prefer. Modify `spmv_2` to compute the performance in MFLOP/s for varying matrix size `N` up to `N=10,000`. Make a plot of performance versus `N`.
3. Optimize your the sparse matrix-vector multiply by implementing loop unrolling. Save the optimized code as `spmv_3`. Compute the performance of the optimized code at varying `N` and plot the performance on the same plot as the unoptimized code.
