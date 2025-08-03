(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-token-not-found (err u102))
(define-constant err-invalid-percentage (err u103))
(define-constant err-property-exists (err u104))
(define-constant err-insufficient-funds (err u105))
(define-constant err-invalid-valuation (err u106))
(define-constant err-voting-closed (err u107))
(define-constant err-already-voted (err u108))
(define-constant err-proposal-not-found (err u109))

(define-non-fungible-token property uint)

(define-map property-details
    uint
    {
        name: (string-ascii 64),
        location: (string-ascii 256),
        total-shares: uint,
        price-per-share: uint,
        revenue-pool: uint,
        active: bool,
    }
)

(define-map share-ownership
    {
        property-id: uint,
        owner: principal,
    }
    uint
)

(define-map property-revenue-claims
    {
        property-id: uint,
        owner: principal,
    }
    uint
)

(define-map property-valuations
    {
        property-id: uint,
        timestamp: uint,
    }
    uint
)

(define-map proposals
    uint
    {
        property-id: uint,
        title: (string-ascii 128),
        description: (string-ascii 512),
        voting-end-height: uint,
        yes-votes: uint,
        no-votes: uint,
        executed: bool,
    }
)

(define-map proposal-votes
    {
        proposal-id: uint,
        voter: principal,
    }
    bool
)

(define-data-var last-property-id uint u0)
(define-data-var total-revenue uint u0)
(define-data-var last-proposal-id uint u0)

(define-read-only (get-last-token-id)
    (ok (var-get last-property-id))
)

(define-read-only (get-token-uri (token-id uint))
    (ok none)
)

(define-read-only (get-owner (token-id uint))
    (ok (nft-get-owner? property token-id))
)

(define-read-only (get-property-details (property-id uint))
    (map-get? property-details property-id)
)

(define-read-only (get-shares
        (property-id uint)
        (owner principal)
    )
    (default-to u0
        (map-get? share-ownership {
            property-id: property-id,
            owner: owner,
        })
    )
)

(define-read-only (get-claimable-revenue
        (property-id uint)
        (owner principal)
    )
    (default-to u0
        (map-get? property-revenue-claims {
            property-id: property-id,
            owner: owner,
        })
    )
)

(define-read-only (get-property-valuation
        (property-id uint)
        (timestamp uint)
    )
    (map-get? property-valuations {
        property-id: property-id,
        timestamp: timestamp,
    })
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (get-vote-status
        (proposal-id uint)
        (voter principal)
    )
    (is-some (map-get? proposal-votes {
        proposal-id: proposal-id,
        voter: voter,
    }))
)

(define-public (register-property
        (name (string-ascii 64))
        (location (string-ascii 256))
        (total-shares uint)
        (price-per-share uint)
    )
    (let ((new-id (+ (var-get last-property-id) u1)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> total-shares u0) err-invalid-percentage)
        (asserts! (> price-per-share u0) err-invalid-percentage)
        (try! (nft-mint? property new-id contract-owner))
        (map-set property-details new-id {
            name: name,
            location: location,
            total-shares: total-shares,
            price-per-share: price-per-share,
            revenue-pool: u0,
            active: true,
        })
        (var-set last-property-id new-id)
        (ok new-id)
    )
)

(define-public (buy-shares
        (property-id uint)
        (share-count uint)
    )
    (let (
            (property-details-entry (unwrap! (map-get? property-details property-id) err-token-not-found))
            (total-cost (* share-count (get price-per-share property-details-entry)))
            (current-shares (get-shares property-id tx-sender))
        )
        (asserts! (get active property-details-entry) err-token-not-found)
        (asserts!
            (<= (+ current-shares share-count)
                (get total-shares property-details-entry)
            )
            err-invalid-percentage
        )
        (try! (stx-transfer? total-cost tx-sender contract-owner))
        (map-set share-ownership {
            property-id: property-id,
            owner: tx-sender,
        }
            (+ current-shares share-count)
        )
        (ok true)
    )
)

(define-public (transfer-shares
        (property-id uint)
        (recipient principal)
        (share-count uint)
    )
    (let ((sender-shares (get-shares property-id tx-sender)))
        (asserts! (>= sender-shares share-count) err-insufficient-funds)
        (map-set share-ownership {
            property-id: property-id,
            owner: tx-sender,
        }
            (- sender-shares share-count)
        )
        (map-set share-ownership {
            property-id: property-id,
            owner: recipient,
        }
            (+ (get-shares property-id recipient) share-count)
        )
        (ok true)
    )
)

(define-public (add-revenue
        (property-id uint)
        (amount uint)
    )
    (let ((property-details-entry (unwrap! (map-get? property-details property-id) err-token-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set property-details property-id
            (merge property-details-entry { revenue-pool: (+ (get revenue-pool property-details-entry) amount) })
        )
        (ok true)
    )
)

(define-public (claim-revenue (property-id uint))
    (let (
            (property-details-entry (unwrap! (map-get? property-details property-id) err-token-not-found))
            (shares (get-shares property-id tx-sender))
            (total-shares (get total-shares property-details-entry))
            (revenue-pool (get revenue-pool property-details-entry))
            (claimable-amount (/ (* shares revenue-pool) total-shares))
        )
        (asserts! (> shares u0) err-not-token-owner)
        (asserts! (> claimable-amount u0) err-insufficient-funds)
        (try! (as-contract (stx-transfer? claimable-amount tx-sender tx-sender)))
        (map-set property-details property-id
            (merge property-details-entry { revenue-pool: (- revenue-pool claimable-amount) })
        )
        (ok claimable-amount)
    )
)

(define-public (deactivate-property (property-id uint))
    (let ((property-entry (unwrap! (map-get? property-details property-id) err-token-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set property-details property-id
            (merge property-entry { active: false })
        )
        (ok true)
    )
)

(define-public (update-property-valuation
        (property-id uint)
        (valuation uint)
    )
    (let ((property-entry (unwrap! (map-get? property-details property-id) err-token-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> valuation u0) err-invalid-valuation)
        (map-set property-valuations {
            property-id: property-id,
            timestamp: stacks-block-height,
        }
            valuation
        )
        (ok true)
    )
)

(define-public (calculate-valuation-change
        (property-id uint)
        (old-timestamp uint)
        (new-timestamp uint)
    )
    (let (
            (old-valuation (unwrap!
                (map-get? property-valuations {
                    property-id: property-id,
                    timestamp: old-timestamp,
                })
                err-token-not-found
            ))
            (new-valuation (unwrap!
                (map-get? property-valuations {
                    property-id: property-id,
                    timestamp: new-timestamp,
                })
                err-token-not-found
            ))
        )
        (asserts! (> new-timestamp old-timestamp) err-invalid-valuation)
        (ok (- new-valuation old-valuation))
    )
)

(define-public (create-proposal
        (property-id uint)
        (title (string-ascii 128))
        (description (string-ascii 512))
        (voting-duration uint)
    )
    (let ((new-proposal-id (+ (var-get last-proposal-id) u1)))
        (asserts! (is-some (map-get? property-details property-id))
            err-token-not-found
        )
        (asserts! (> (get-shares property-id tx-sender) u0) err-not-token-owner)
        (asserts! (> voting-duration u0) err-invalid-percentage)
        (map-set proposals new-proposal-id {
            property-id: property-id,
            title: title,
            description: description,
            voting-end-height: (+ stacks-block-height voting-duration),
            yes-votes: u0,
            no-votes: u0,
            executed: false,
        })
        (var-set last-proposal-id new-proposal-id)
        (ok new-proposal-id)
    )
)

(define-public (vote-on-proposal
        (proposal-id uint)
        (vote bool)
    )
    (let (
            (proposal-entry (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
            (voter-shares (get-shares (get property-id proposal-entry) tx-sender))
        )
        (asserts! (> voter-shares u0) err-not-token-owner)
        (asserts! (< stacks-block-height (get voting-end-height proposal-entry))
            err-voting-closed
        )
        (asserts! (not (get-vote-status proposal-id tx-sender)) err-already-voted)
        (map-set proposal-votes {
            proposal-id: proposal-id,
            voter: tx-sender,
        }
            true
        )
        (if vote
            (map-set proposals proposal-id
                (merge proposal-entry { yes-votes: (+ (get yes-votes proposal-entry) voter-shares) })
            )
            (map-set proposals proposal-id
                (merge proposal-entry { no-votes: (+ (get no-votes proposal-entry) voter-shares) })
            )
        )
        (ok true)
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let (
            (proposal-entry (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
            (property-details-entry (unwrap! (map-get? property-details (get property-id proposal-entry))
                err-token-not-found
            ))
            (total-shares (get total-shares property-details-entry))
            (quorum-threshold (/ total-shares u2))
        )
        (asserts! (>= stacks-block-height (get voting-end-height proposal-entry))
            err-voting-closed
        )
        (asserts! (not (get executed proposal-entry)) err-property-exists)
        (asserts!
            (> (get yes-votes proposal-entry) (get no-votes proposal-entry))
            err-insufficient-funds
        )
        (asserts! (> (get yes-votes proposal-entry) quorum-threshold)
            err-insufficient-funds
        )
        (map-set proposals proposal-id (merge proposal-entry { executed: true }))
        (ok true)
    )
)
