;; Streaming Analytics Dashboard
;; Provides comprehensive insights and metrics for the streaming platform

;; Error constants
(define-constant err-unauthorized (err u400))
(define-constant err-not-found (err u401))
(define-constant err-invalid-period (err u402))
(define-constant err-invalid-data (err u403))

;; Analytics period constants
(define-constant PERIOD-DAILY u144)     ;; Approximately 1 day in blocks
(define-constant PERIOD-WEEKLY u1008)   ;; Approximately 1 week in blocks  
(define-constant PERIOD-MONTHLY u4320)  ;; Approximately 1 month in blocks

;; Data variables
(define-data-var analytics-enabled bool true)
(define-data-var last-report-id uint u0)

;; Daily analytics snapshots
(define-map daily-analytics
  { date-block: uint }
  {
    total-streams: uint,
    total-revenue: uint,
    active-listeners: uint,
    top-song-id: uint,
    avg-stream-price: uint
  }
)

;; Artist performance metrics
(define-map artist-metrics
  { artist-id: uint, period-start: uint }
  {
    streams-count: uint,
    revenue-earned: uint,
    unique-listeners: uint,
    avg-price-per-stream: uint,
    growth-rate: uint
  }
)

;; Song performance tracking
(define-map song-metrics
  { song-id: uint, period-start: uint }
  {
    stream-count: uint,
    revenue-generated: uint,
    listener-count: uint,
    peak-price: uint,
    retention-rate: uint
  }
)

;; Listener behavior patterns
(define-map listener-analytics
  { listener: principal, period-start: uint }
  {
    songs-streamed: uint,
    total-spent: uint,
    favorite-genre: uint,
    session-count: uint,
    avg-session-duration: uint
  }
)

;; Performance reports
(define-map analytics-reports
  { report-id: uint }
  {
    report-type: uint,
    entity-id: uint,
    period-start: uint,
    period-end: uint,
    generated-at: uint,
    summary-data: {
      metric1: uint,
      metric2: uint,
      metric3: uint
    }
  }
)

;; Read-only functions
(define-read-only (get-daily-analytics (date-block uint))
  (map-get? daily-analytics { date-block: date-block })
)

(define-read-only (get-artist-metrics (artist-id uint) (period-start uint))
  (map-get? artist-metrics { artist-id: artist-id, period-start: period-start })
)

(define-read-only (get-song-metrics (song-id uint) (period-start uint))
  (map-get? song-metrics { song-id: song-id, period-start: period-start })
)

(define-read-only (get-listener-analytics (listener principal) (period-start uint))
  (map-get? listener-analytics { listener: listener, period-start: period-start })
)

(define-read-only (get-analytics-report (report-id uint))
  (map-get? analytics-reports { report-id: report-id })
)

;; Record a streaming event for analytics
(define-public (record-stream-analytics (song-id uint) (artist-id uint) (amount-paid uint))
  (let
    (
      (current-block stacks-block-height)
      (period-start (- current-block (mod current-block PERIOD-DAILY)))
    )
    ;; Update daily metrics
    (update-daily-metrics current-block amount-paid)
    
    ;; Update artist metrics  
    (update-artist-metrics artist-id period-start amount-paid)
    
    ;; Update song metrics
    (update-song-metrics song-id period-start amount-paid)
    
    ;; Update listener metrics
    (update-listener-metrics tx-sender period-start amount-paid)
    
    (ok true)
  )
)

;; Update daily platform metrics
(define-private (update-daily-metrics (current-block uint) (revenue uint))
  (let
    (
      (date-key (- current-block (mod current-block PERIOD-DAILY)))
      (existing-data (default-to 
        { total-streams: u0, total-revenue: u0, active-listeners: u0, top-song-id: u0, avg-stream-price: u0 }
        (map-get? daily-analytics { date-block: date-key })
      ))
      (new-streams (+ (get total-streams existing-data) u1))
      (new-revenue (+ (get total-revenue existing-data) revenue))
    )
    (map-set daily-analytics
      { date-block: date-key }
      {
        total-streams: new-streams,
        total-revenue: new-revenue,
        active-listeners: (+ (get active-listeners existing-data) u1),
        top-song-id: (get top-song-id existing-data),
        avg-stream-price: (/ new-revenue new-streams)
      }
    )
    true
  )
)

;; Update artist performance metrics  
(define-private (update-artist-metrics (artist-id uint) (period-start uint) (revenue uint))
  (let
    (
      (existing-metrics (default-to
        { streams-count: u0, revenue-earned: u0, unique-listeners: u0, avg-price-per-stream: u0, growth-rate: u0 }
        (map-get? artist-metrics { artist-id: artist-id, period-start: period-start })
      ))
      (new-streams (+ (get streams-count existing-metrics) u1))
      (new-revenue (+ (get revenue-earned existing-metrics) revenue))
    )
    (map-set artist-metrics
      { artist-id: artist-id, period-start: period-start }
      {
        streams-count: new-streams,
        revenue-earned: new-revenue,
        unique-listeners: (+ (get unique-listeners existing-metrics) u1),
        avg-price-per-stream: (/ new-revenue new-streams),
        growth-rate: (get growth-rate existing-metrics)
      }
    )
    true
  )
)

;; Update song performance metrics
(define-private (update-song-metrics (song-id uint) (period-start uint) (revenue uint))
  (let
    (
      (existing-metrics (default-to
        { stream-count: u0, revenue-generated: u0, listener-count: u0, peak-price: u0, retention-rate: u0 }
        (map-get? song-metrics { song-id: song-id, period-start: period-start })
      ))
      (new-streams (+ (get stream-count existing-metrics) u1))
      (new-revenue (+ (get revenue-generated existing-metrics) revenue))
    )
    (map-set song-metrics
      { song-id: song-id, period-start: period-start }
      {
        stream-count: new-streams,
        revenue-generated: new-revenue,
        listener-count: (+ (get listener-count existing-metrics) u1),
        peak-price: (if (> revenue (get peak-price existing-metrics)) revenue (get peak-price existing-metrics)),
        retention-rate: (get retention-rate existing-metrics)
      }
    )
    true
  )
)

;; Update listener behavior metrics
(define-private (update-listener-metrics (listener principal) (period-start uint) (amount-spent uint))
  (let
    (
      (existing-analytics (default-to
        { songs-streamed: u0, total-spent: u0, favorite-genre: u0, session-count: u0, avg-session-duration: u0 }
        (map-get? listener-analytics { listener: listener, period-start: period-start })
      ))
    )
    (map-set listener-analytics
      { listener: listener, period-start: period-start }
      {
        songs-streamed: (+ (get songs-streamed existing-analytics) u1),
        total-spent: (+ (get total-spent existing-analytics) amount-spent),
        favorite-genre: (get favorite-genre existing-analytics),
        session-count: (+ (get session-count existing-analytics) u1),
        avg-session-duration: (get avg-session-duration existing-analytics)
      }
    )
    true
  )
)

;; Generate analytics report
(define-public (generate-analytics-report (report-type uint) (entity-id uint) (period-blocks uint))
  (let
    (
      (current-block stacks-block-height)
      (period-start (- current-block period-blocks))
      (new-report-id (+ (var-get last-report-id) u1))
    )
    (asserts! (var-get analytics-enabled) err-unauthorized)
    (asserts! (> period-blocks u0) err-invalid-period)
    
    (var-set last-report-id new-report-id)
    (map-set analytics-reports
      { report-id: new-report-id }
      {
        report-type: report-type,
        entity-id: entity-id,
        period-start: period-start,
        period-end: current-block,
        generated-at: current-block,
        summary-data: {
          metric1: u0,
          metric2: u0,
          metric3: u0
        }
      }
    )
    (ok new-report-id)
  )
)

;; Get trending songs based on recent activity
(define-read-only (get-trending-songs (period-blocks uint))
  (let
    (
      (current-block stacks-block-height)
      (period-start (- current-block period-blocks))
    )
    (ok {
      period-start: period-start,
      period-end: current-block,
      trending-calculation-method: "stream-velocity",
      sample-data: { song-id: u1, trend-score: u100 }
    })
  )
)

;; Get listener engagement metrics
(define-read-only (get-engagement-metrics (listener principal) (period-blocks uint))
  (let
    (
      (current-block stacks-block-height)
      (period-start (- current-block period-blocks))
      (analytics-data (map-get? listener-analytics { listener: listener, period-start: period-start }))
    )
    (match analytics-data
      data (ok {
        total-streams: (get songs-streamed data),
        engagement-score: (/ (get songs-streamed data) period-blocks),
        spending-velocity: (/ (get total-spent data) period-blocks),
        session-frequency: (get session-count data)
      })
      (ok { total-streams: u0, engagement-score: u0, spending-velocity: u0, session-frequency: u0 })
    )
  )
)

;; Calculate artist performance score
(define-read-only (get-artist-performance-score (artist-id uint) (period-blocks uint))
  (let
    (
      (current-block stacks-block-height)
      (period-start (- current-block period-blocks))
      (metrics (map-get? artist-metrics { artist-id: artist-id, period-start: period-start }))
    )
    (match metrics
      data 
        (let
          (
            (stream-score (/ (get streams-count data) u10))
            (revenue-score (/ (get revenue-earned data) u1000))
            (engagement-score (get unique-listeners data))
          )
          (ok (+ stream-score (+ revenue-score engagement-score)))
        )
      (ok u0)
    )
  )
)

;; Admin functions
(define-public (toggle-analytics (enabled bool))
  (begin
    (var-set analytics-enabled enabled)
    (ok enabled)
  )
)

(define-public (reset-analytics-period)
  (begin
    (var-set last-report-id u0)
    (ok true)
  )
)
