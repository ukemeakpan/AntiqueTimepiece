;; AntiqueTimepiece: Vintage Watch and Clock Authentication Registry
;; Version: 1.0.0

(define-constant ERR-ACCESS-FORBIDDEN (err u1))
(define-constant ERR-TIMEPIECE-NOT-FOUND (err u2))
(define-constant ERR-DUPLICATE-REGISTRATION (err u3))
(define-constant ERR-INVALID-STATUS (err u4))
(define-constant ERR-INVALID-MANUFACTURE-YEAR (err u5))
(define-constant ERR-INVALID-TIMEPIECE-TYPE (err u6))
(define-constant ERR-INVALID-MECHANISM (err u7))
(define-constant ERR-INVALID-MODEL-NAME (err u8))
(define-constant ERR-INVALID-MAKER (err u9))

(define-constant MIN-MANUFACTURE-YEAR u1650)

(define-data-var next-timepiece-id uint u1)

(define-map antique-timepieces
    uint
    {
        curator: principal,
        model-name: (string-utf8 90),
        maker: (string-utf8 160),
        timepiece-type: (string-utf8 20),
        mechanism: (string-utf8 25),
        status: (string-utf8 15),
        manufacture-year: uint
    }
)

(define-private (validate-timepiece-type (timepiece-type (string-utf8 20)))
    (or 
        (is-eq timepiece-type u"Pocket Watch")
        (is-eq timepiece-type u"Wristwatch")
        (is-eq timepiece-type u"Mantel Clock")
        (is-eq timepiece-type u"Wall Clock")
        (is-eq timepiece-type u"Grandfather Clock")
        (is-eq timepiece-type u"Marine Chronometer")
    )
)

(define-private (validate-mechanism (mechanism (string-utf8 25)))
    (or 
        (is-eq mechanism u"Mechanical")
        (is-eq mechanism u"Automatic")
        (is-eq mechanism u"Spring-driven")
        (is-eq mechanism u"Pendulum")
        (is-eq mechanism u"Quartz")
    )
)

(define-private (validate-text-field (text (string-utf8 160)) (min-chars uint) (max-chars uint))
    (let 
        (
            (field-length (len text))
        )
        (and 
            (>= field-length min-chars)
            (<= field-length max-chars)
        )
    )
)

(define-public (register-timepiece 
    (model-name (string-utf8 90))
    (maker (string-utf8 160))
    (timepiece-type (string-utf8 20))
    (mechanism (string-utf8 25))
    (manufacture-year uint)
)
    (let
        (
            (timepiece-id (var-get next-timepiece-id))
        )
        (asserts! (validate-text-field model-name u3 u90) ERR-INVALID-MODEL-NAME)
        (asserts! (validate-text-field maker u5 u160) ERR-INVALID-MAKER)
        (asserts! (>= manufacture-year MIN-MANUFACTURE-YEAR) ERR-INVALID-MANUFACTURE-YEAR)
        (asserts! (validate-timepiece-type timepiece-type) ERR-INVALID-TIMEPIECE-TYPE)
        (asserts! (validate-mechanism mechanism) ERR-INVALID-MECHANISM)
        
        (map-set antique-timepieces timepiece-id {
            curator: tx-sender,
            model-name: model-name,
            maker: maker,
            timepiece-type: timepiece-type,
            mechanism: mechanism,
            status: u"authenticated",
            manufacture-year: manufacture-year
        })
        (var-set next-timepiece-id (+ timepiece-id u1))
        (ok timepiece-id)
    )
)

(define-public (auction-timepiece (timepiece-id uint))
    (let
        (
            (timepiece (unwrap! (map-get? antique-timepieces timepiece-id) ERR-TIMEPIECE-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get curator timepiece)) ERR-ACCESS-FORBIDDEN)
        (asserts! (is-eq (get status timepiece) u"authenticated") ERR-INVALID-STATUS)
        (ok (map-set antique-timepieces timepiece-id (merge timepiece { status: u"at-auction" })))
    )
)

(define-read-only (get-timepiece-data (timepiece-id uint))
    (ok (map-get? antique-timepieces timepiece-id))
)

(define-read-only (get-curator (timepiece-id uint))
    (ok (get curator (unwrap! (map-get? antique-timepieces timepiece-id) ERR-TIMEPIECE-NOT-FOUND)))
)