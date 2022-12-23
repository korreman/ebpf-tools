;; Simple Euclids GCD, demonstrating control flow
  mov r0 15
  mov r1 27

loop:
  jeq r1 0, done
  mov r2 r0
  mod r2 r1
  mov r0 r1
  mov r1 r2
  ja loop

done:
  exit
