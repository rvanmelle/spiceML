
;; (spi:model 'BJT type: "ebers-moll" io: 1e-15 beta: 100 vt: 25m)

(spi:vsrc 'v1 1 0 dc: 2.0)
(spi:res 'r1 1 2 10.0)
(spi:res 'r2 2 0 5.0)
;(spi:cap 'c1 2 3 1u)
;(spi:ind 'l1 3 0 1m)
(spi:dc 'dc1)
(spi:vdc 'dc1 1)
