val create_single_spline :
  float ->
    float list ->
      float ->
        float list ->
          (float -> float)

val create_spline :
  (float * float list) list ->
    (float -> float)
    
val create_auto_spline :
  float ->
    float list ->
      (float * float) list ->
        float ->
          float list ->
            (float -> float)

val create_auto_matrix_spline :
  float ->
    Matrix.t list ->
      (float * Matrix.t) list ->
        float ->
          Matrix.t list ->
            (float -> Matrix.t)
