;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XML; readtable: runes; Encoding: utf-8; -*-

(in-package :xml)

#.(funcall 
   (compile 
    nil
    '(lambda ()
      (let ((*max* #xD800))
        (labels
            ((name-start-rune-p (rune)
               (or (letter-rune-p rune)
                   (= #.(char-code #\_) rune)
                   (= #.(char-code #\:) rune)))

             (name-rune-p (rune)
               (or (letter-rune-p rune)
                   (digit-rune-p* rune)
                   (= rune #.(char-code #\.))
                   (= rune #.(char-code #\-))
                   (= rune #.(char-code #\_))
                   (= rune #.(char-code #\:))
                   (combining-rune-p rune)
                   (extender-rune-p rune)))

             (letter-rune-p (rune)
               (or (base-rune-p rune)
                   (ideographic-rune-p rune)))

             (digit-rune-p* (rune)
               (or (<= 48 rune 57)
                   (<= 1632 rune 1641)
                   (<= 1776 rune 1785)
                   (<= 2406 rune 2415)
                   (<= 2534 rune 2543)
                   (<= 2662 rune 2671)
                   (<= 2790 rune 2799)
                   (<= 2918 rune 2927)
                   (<= 3047 rune 3055)
                   (<= 3174 rune 3183)
                   (<= 3302 rune 3311)
                   (<= 3430 rune 3439)
                   (<= 3664 rune 3673)
                   (<= 3792 rune 3801)
                   (<= 3872 rune 3881)))


             (combining-rune-p (rune)
               (or (<= 768 rune 837)
                   (<= 864 rune 865)
                   (<= 1155 rune 1158)
                   (<= 1425 rune 1441)
                   (<= 1443 rune 1465)
                   (<= 1467 rune 1469)
                   (= 1471 rune)
                   (<= 1473 rune 1474)
                   (= 1476 rune)
                   (<= 1611 rune 1618)
                   (= 1648 rune)
                   (<= 1750 rune 1756)
                   (<= 1757 rune 1759)
                   (<= 1760 rune 1764)
                   (<= 1767 rune 1768)
                   (<= 1770 rune 1773)
                   (<= 2305 rune 2307)
                   (= 2364 rune)
                   (<= 2366 rune 2380)
                   (= 2381 rune)
                   (<= 2385 rune 2388)
                   (<= 2402 rune 2403)
                   (<= 2433 rune 2435)
                   (= 2492 rune)
                   (= 2494 rune)
                   (= 2495 rune)
                   (<= 2496 rune 2500)
                   (<= 2503 rune 2504)
                   (<= 2507 rune 2509)
                   (= 2519 rune)
                   (<= 2530 rune 2531)
                   (= 2562 rune)
                   (= 2620 rune)
                   (= 2622 rune)
                   (= 2623 rune)
                   (<= 2624 rune 2626)
                   (<= 2631 rune 2632)
                   (<= 2635 rune 2637)
                   (<= 2672 rune 2673)
                   (<= 2689 rune 2691)
                   (= 2748 rune)
                   (<= 2750 rune 2757)
                   (<= 2759 rune 2761)
                   (<= 2763 rune 2765)
                   (<= 2817 rune 2819)
                   (= 2876 rune)
                   (<= 2878 rune 2883)
                   (<= 2887 rune 2888)
                   (<= 2891 rune 2893)
                   (<= 2902 rune 2903)
                   (<= 2946 rune 2947)
                   (<= 3006 rune 3010)
                   (<= 3014 rune 3016)
                   (<= 3018 rune 3021)
                   (= 3031 rune)
                   (<= 3073 rune 3075)
                   (<= 3134 rune 3140)
                   (<= 3142 rune 3144)
                   (<= 3146 rune 3149)
                   (<= 3157 rune 3158)
                   (<= 3202 rune 3203)
                   (<= 3262 rune 3268)
                   (<= 3270 rune 3272)
                   (<= 3274 rune 3277)
                   (<= 3285 rune 3286)
                   (<= 3330 rune 3331)
                   (<= 3390 rune 3395)
                   (<= 3398 rune 3400)
                   (<= 3402 rune 3405)
                   (= 3415 rune)
                   (= 3633 rune)
                   (<= 3636 rune 3642)
                   (<= 3655 rune 3662)
                   (= 3761 rune)
                   (<= 3764 rune 3769)
                   (<= 3771 rune 3772)
                   (<= 3784 rune 3789)
                   (<= 3864 rune 3865)
                   (= 3893 rune)
                   (= 3895 rune)
                   (= 3897 rune)
                   (= 3902 rune)
                   (= 3903 rune)
                   (<= 3953 rune 3972)
                   (<= 3974 rune 3979)
                   (<= 3984 rune 3989)
                   (= 3991 rune)
                   (<= 3993 rune 4013)
                   (<= 4017 rune 4023)
                   (= 4025 rune)
                   (<= 8400 rune 8412)
                   (= 8417 rune)
                   (<= 12330 rune 12335)
                   (= 12441 rune)
                   (= 12442 rune)))

             (extender-rune-p (rune)
               (or
                (= 183 rune)
                (= 720 rune)
                (= 721 rune)
                (= 903 rune)
                (= 1600 rune)
                (= 3654 rune)
                (= 3782 rune)
                (= 12293 rune)
                (<= 12337 rune 12341)
                (<= 12445 rune 12446)
                (<= 12540 rune 12542)))

             (base-rune-p (rune)
               (or 
                (<= 65 rune 90) (<= 97 rune 122) (<= 192 rune 214) (<= 216 rune 246) (<= 248 rune 255) (<= 256 rune 305)
                (<= 308 rune 318) (<= 321 rune 328) (<= 330 rune 382) (<= 384 rune 451) (<= 461 rune 496) (<= 500 rune 501)
                (<= 506 rune 535) (<= 592 rune 680) (<= 699 rune 705) (= 902 rune) (<= 904 rune 906) (= 908 rune)
                (<= 910 rune 929) (<= 931 rune 974) (<= 976 rune 982) (= 986 rune) (= 988 rune) (= 990 rune) (= 992 rune)
                (<= 994 rune 1011) (<= 1025 rune 1036) (<= 1038 rune 1103) (<= 1105 rune 1116) (<= 1118 rune 1153)
                (<= 1168 rune 1220) (<= 1223 rune 1224) (<= 1227 rune 1228) (<= 1232 rune 1259) (<= 1262 rune 1269)
                (<= 1272 rune 1273) (<= 1329 rune 1366) (= 1369 rune) (<= 1377 rune 1414) (<= 1488 rune 1514)
                (<= 1520 rune 1522) (<= 1569 rune 1594) (<= 1601 rune 1610) (<= 1649 rune 1719) (<= 1722 rune 1726)
                (<= 1728 rune 1742) (<= 1744 rune 1747) (= 1749 rune) (<= 1765 rune 1766) (<= 2309 rune 2361) (= 2365 rune)
                (<= 2392 rune 2401) (<= 2437 rune 2444) (<= 2447 rune 2448) (<= 2451 rune 2472) (<= 2474 rune 2480)
                (= 2482 rune) (<= 2486 rune 2489) (<= 2524 rune 2525) (<= 2527 rune 2529) (<= 2544 rune 2545)
                (<= 2565 rune 2570) (<= 2575 rune 2576) (<= 2579 rune 2600) (<= 2602 rune 2608) (<= 2610 rune 2611)
                (<= 2613 rune 2614) (<= 2616 rune 2617) (<= 2649 rune 2652) (= 2654 rune) (<= 2674 rune 2676)
                (<= 2693 rune 2699) (= 2701 rune) (<= 2703 rune 2705) (<= 2707 rune 2728) (<= 2730 rune 2736)
                (<= 2738 rune 2739) (<= 2741 rune 2745) (= 2749 rune) (= 2784 rune) (<= 2821 rune 2828) (<= 2831 rune 2832)
                (<= 2835 rune 2856) (<= 2858 rune 2864) (<= 2866 rune 2867) (<= 2870 rune 2873) (= 2877 rune)
                (<= 2908 rune 2909) (<= 2911 rune 2913) (<= 2949 rune 2954) (<= 2958 rune 2960) (<= 2962 rune 2965)
                (<= 2969 rune 2970) (= 2972 rune) (<= 2974 rune 2975) (<= 2979 rune 2980) (<= 2984 rune 2986)
                (<= 2990 rune 2997) (<= 2999 rune 3001) (<= 3077 rune 3084) (<= 3086 rune 3088) (<= 3090 rune 3112)
                (<= 3114 rune 3123) (<= 3125 rune 3129) (<= 3168 rune 3169) (<= 3205 rune 3212) (<= 3214 rune 3216)
                (<= 3218 rune 3240) (<= 3242 rune 3251) (<= 3253 rune 3257) (= 3294 rune) (<= 3296 rune 3297)
                (<= 3333 rune 3340) (<= 3342 rune 3344) (<= 3346 rune 3368) (<= 3370 rune 3385) (<= 3424 rune 3425)
                (<= 3585 rune 3630) (= 3632 rune) (<= 3634 rune 3635) (<= 3648 rune 3653) (<= 3713 rune 3714) (= 3716 rune)
                (<= 3719 rune 3720) (= 3722 rune) (= 3725 rune) (<= 3732 rune 3735) (<= 3737 rune 3743) (<= 3745 rune 3747)
                (= 3749 rune) (= 3751 rune) (<= 3754 rune 3755) (<= 3757 rune 3758) (= 3760 rune) (<= 3762 rune 3763) (= 3773 rune)
                (<= 3776 rune 3780) (<= 3904 rune 3911) (<= 3913 rune 3945) (<= 4256 rune 4293) (<= 4304 rune 4342)
                (= 4352 rune) (<= 4354 rune 4355) (<= 4357 rune 4359) (= 4361 rune) (<= 4363 rune 4364) (<= 4366 rune 4370)
                (= 4412 rune) (= 4414 rune) (= 4416 rune) (= 4428 rune) (= 4430 rune) (= 4432 rune) (<= 4436 rune 4437) (= 4441 rune)
                (<= 4447 rune 4449) (= 4451 rune) (= 4453 rune) (= 4455 rune) (= 4457 rune) (<= 4461 rune 4462) (<= 4466 rune 4467)
                (= 4469 rune) (= 4510 rune) (= 4520 rune) (= 4523 rune) (<= 4526 rune 4527) (<= 4535 rune 4536) (= 4538 rune)
                (<= 4540 rune 4546) (= 4587 rune) (= 4592 rune) (= 4601 rune) (<= 7680 rune 7835) (<= 7840 rune 7929)
                (<= 7936 rune 7957) (<= 7960 rune 7965) (<= 7968 rune 8005) (<= 8008 rune 8013) (<= 8016 rune 8023)
                (= 8025 rune) (= 8027 rune) (= 8029 rune) (<= 8031 rune 8061) (<= 8064 rune 8116) (<= 8118 rune 8124) (= 8126 rune)
                (<= 8130 rune 8132) (<= 8134 rune 8140) (<= 8144 rune 8147) (<= 8150 rune 8155) (<= 8160 rune 8172)
                (<= 8178 rune 8180) (<= 8182 rune 8188) (= 8486 rune) (<= 8490 rune 8491) (= 8494 rune) (<= 8576 rune 8578)
                (<= 12353 rune 12436) (<= 12449 rune 12538) (<= 12549 rune 12588) (<= 44032 rune 55203)))

             (ideographic-rune-p (rune)
               (or (<= 19968 rune 40869) (= 12295 rune) (<= 12321 rune 12329)))


             (predicate-to-bv (p)
               (let ((r (make-array *max* :element-type 'bit :initial-element 0)))
                 (dotimes (i #x10000 r)
                   (when (funcall p i)
                     (setf (aref r i) 1))))) )

          `(progn
             (defsubst name-rune-p (rune)
               (let ((code (rune-code rune)))
                 (and (<= 0 code ,*max*)
                      (locally (declare (optimize (safety 0) (speed 3)))
                        (= 1 (sbit ',(predicate-to-bv #'name-rune-p)
                                   (the fixnum code)))))))
             (defsubst name-start-rune-p (rune)
               (let ((code (rune-code rune)))
                 (and (<= 0 code ,*max*)
                      (locally (declare (optimize (safety 0) (speed 3)))
                        (= 1 (sbit ',(predicate-to-bv #'name-start-rune-p)
                                   (the fixnum code))))))))) ))))
