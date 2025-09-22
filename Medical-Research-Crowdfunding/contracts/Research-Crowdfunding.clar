;; Medical Research Crowdfunding Smart Contract
;; Enables transparent funding for specific health research projects

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_PROJECT_NOT_FOUND (err u101))
(define-constant ERR_PROJECT_EXPIRED (err u102))
(define-constant ERR_PROJECT_FUNDED (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_INVALID_AMOUNT (err u105))
(define-constant ERR_ALREADY_REFUNDED (err u106))
(define-constant ERR_PROJECT_ACTIVE (err u107))
(define-constant ERR_MILESTONE_NOT_FOUND (err u108))
(define-constant ERR_MILESTONE_COMPLETED (err u109))

;; Data Variables
(define-data-var project-counter uint u0)
(define-data-var platform-fee-rate uint u250) ;; 2.5% (250 basis points)

;; Data Maps
(define-map projects
    uint
    {
        creator: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        target-amount: uint,
        raised-amount: uint,
        deadline: uint,
        category: (string-ascii 50),
        status: (string-ascii 20), ;; "active", "funded", "expired", "completed"
        research-institution: (string-ascii 100),
        created-at: uint
    }
)

(define-map contributions
    {project-id: uint, contributor: principal}
    {
        amount: uint,
        timestamp: uint,
        refunded: bool
    }
)

(define-map milestones
    {project-id: uint, milestone-id: uint}
    {
        description: (string-ascii 200),
        target-amount: uint,
        completed: bool,
        completion-evidence: (optional (string-ascii 200)),
        approved-by-votes: uint,
        total-votes: uint
    }
)

(define-map project-milestones-count uint uint)
(define-map contributor-votes {project-id: uint, milestone-id: uint, voter: principal} bool)
(define-map project-contributors uint (list 100 principal))
(define-map user-contributions principal (list 50 uint))

;; Public Functions

;; Create a new research project
(define-public (create-project 
    (title (string-ascii 100))
    (description (string-ascii 500))
    (target-amount uint)
    (duration-blocks uint)
    (category (string-ascii 50))
    (research-institution (string-ascii 100)))
    (let ((project-id (+ (var-get project-counter) u1))
          (deadline (+ block-height duration-blocks)))
        (asserts! (> target-amount u0) ERR_INVALID_AMOUNT)
        (map-set projects project-id
            {
                creator: tx-sender,
                title: title,
                description: description,
                target-amount: target-amount,
                raised-amount: u0,
                deadline: deadline,
                category: category,
                status: "active",
                research-institution: research-institution,
                created-at: block-height
            }
        )
        (var-set project-counter project-id)
        (map-set project-milestones-count project-id u0)
        (ok project-id)
    )
)

;; Add milestone to a project
(define-public (add-milestone
    (project-id uint)
    (description (string-ascii 200))
    (target-amount uint))
    (let ((project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
          (milestone-count (default-to u0 (map-get? project-milestones-count project-id)))
          (new-milestone-id (+ milestone-count u1)))
        (asserts! (is-eq (get creator project) tx-sender) ERR_NOT_AUTHORIZED)
        (asserts! (is-eq (get status project) "active") ERR_PROJECT_ACTIVE)
        (asserts! (> target-amount u0) ERR_INVALID_AMOUNT)
        
        (map-set milestones {project-id: project-id, milestone-id: new-milestone-id}
            {
                description: description,
                target-amount: target-amount,
                completed: false,
                completion-evidence: none,
                approved-by-votes: u0,
                total-votes: u0
            }
        )
        (map-set project-milestones-count project-id new-milestone-id)
        (ok new-milestone-id)
    )
)

;; Contribute to a project
(define-public (contribute (project-id uint) (amount uint))
    (let ((project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
          (current-contributions (default-to (list) (map-get? project-contributors project-id)))
          (user-contribs (default-to (list) (map-get? user-contributions tx-sender))))
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (asserts! (is-eq (get status project) "active") ERR_PROJECT_ACTIVE)
        (asserts! (<= block-height (get deadline project)) ERR_PROJECT_EXPIRED)
        
        ;; Transfer STX to contract
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        ;; Update contribution record
        (map-set contributions {project-id: project-id, contributor: tx-sender}
            {
                amount: (+ amount (get amount (default-to {amount: u0, timestamp: u0, refunded: false} 
                    (map-get? contributions {project-id: project-id, contributor: tx-sender})))),
                timestamp: block-height,
                refunded: false
            }
        )
        
        ;; Update project raised amount
        (map-set projects project-id
            (merge project {raised-amount: (+ (get raised-amount project) amount)})
        )
        
        ;; Update contributor lists (simplified - in production would need better list management)
        (map-set project-contributors project-id 
            (unwrap-panic (as-max-len? (append current-contributions tx-sender) u100)))
        (map-set user-contributions tx-sender
            (unwrap-panic (as-max-len? (append user-contribs project-id) u50)))
        
        (ok amount)
    )
)

;; Complete milestone with evidence
(define-public (complete-milestone
    (project-id uint)
    (milestone-id uint)
    (evidence (string-ascii 200)))
    (let ((project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
          (milestone (unwrap! (map-get? milestones {project-id: project-id, milestone-id: milestone-id}) ERR_MILESTONE_NOT_FOUND)))
        (asserts! (is-eq (get creator project) tx-sender) ERR_NOT_AUTHORIZED)
        (asserts! (not (get completed milestone)) ERR_MILESTONE_COMPLETED)
        
        (map-set milestones {project-id: project-id, milestone-id: milestone-id}
            (merge milestone {
                completion-evidence: (some evidence),
                completed: true
            })
        )
        (ok true)
    )
)

;; Vote on milestone completion
(define-public (vote-milestone
    (project-id uint)
    (milestone-id uint)
    (approve bool))
    (let ((milestone (unwrap! (map-get? milestones {project-id: project-id, milestone-id: milestone-id}) ERR_MILESTONE_NOT_FOUND))
          (contribution (unwrap! (map-get? contributions {project-id: project-id, contributor: tx-sender}) ERR_NOT_AUTHORIZED))
          (already-voted (default-to false (map-get? contributor-votes {project-id: project-id, milestone-id: milestone-id, voter: tx-sender}))))
        
        (asserts! (get completed milestone) ERR_MILESTONE_NOT_FOUND)
        (asserts! (> (get amount contribution) u0) ERR_NOT_AUTHORIZED)
        (asserts! (not already-voted) ERR_NOT_AUTHORIZED)
        
        (map-set contributor-votes {project-id: project-id, milestone-id: milestone-id, voter: tx-sender} true)
        
        (map-set milestones {project-id: project-id, milestone-id: milestone-id}
            (merge milestone {
                approved-by-votes: (if approve (+ (get approved-by-votes milestone) u1) (get approved-by-votes milestone)),
                total-votes: (+ (get total-votes milestone) u1)
            })
        )
        (ok true)
    )
)

;; Withdraw funds for completed milestones
(define-public (withdraw-milestone-funds
    (project-id uint)
    (milestone-id uint))
    (let ((project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
          (milestone (unwrap! (map-get? milestones {project-id: project-id, milestone-id: milestone-id}) ERR_MILESTONE_NOT_FOUND))
          (approval-rate (if (> (get total-votes milestone) u0) 
                            (/ (* (get approved-by-votes milestone) u100) (get total-votes milestone)) u0))
          (withdraw-amount (get target-amount milestone))
          (platform-fee (/ (* withdraw-amount (var-get platform-fee-rate)) u10000)))
        
        (asserts! (is-eq (get creator project) tx-sender) ERR_NOT_AUTHORIZED)
        (asserts! (get completed milestone) ERR_MILESTONE_NOT_FOUND)
        (asserts! (>= approval-rate u60) ERR_NOT_AUTHORIZED) ;; 60% approval required
        
        ;; Transfer funds to project creator (minus platform fee)
        (try! (as-contract (stx-transfer? (- withdraw-amount platform-fee) tx-sender (get creator project))))
        
        ;; Transfer platform fee to contract owner
        (try! (as-contract (stx-transfer? platform-fee tx-sender CONTRACT_OWNER)))
        
        (ok withdraw-amount)
    )
)

;; Request refund if project expired and not funded
(define-public (request-refund (project-id uint))
    (let ((project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
          (contribution (unwrap! (map-get? contributions {project-id: project-id, contributor: tx-sender}) ERR_INSUFFICIENT_FUNDS)))
        
        (asserts! (> block-height (get deadline project)) ERR_PROJECT_ACTIVE)
        (asserts! (< (get raised-amount project) (get target-amount project)) ERR_PROJECT_FUNDED)
        (asserts! (not (get refunded contribution)) ERR_ALREADY_REFUNDED)
        
        ;; Mark as refunded
        (map-set contributions {project-id: project-id, contributor: tx-sender}
            (merge contribution {refunded: true}))
        
        ;; Transfer refund
        (try! (as-contract (stx-transfer? (get amount contribution) tx-sender tx-sender)))
        
        (ok (get amount contribution))
    )
)

;; Read-only functions

(define-read-only (get-project (project-id uint))
    (map-get? projects project-id)
)

(define-read-only (get-contribution (project-id uint) (contributor principal))
    (map-get? contributions {project-id: project-id, contributor: contributor})
)

(define-read-only (get-milestone (project-id uint) (milestone-id uint))
    (map-get? milestones {project-id: project-id, milestone-id: milestone-id})
)

(define-read-only (get-project-count)
    (var-get project-counter)
)

(define-read-only (get-platform-fee-rate)
    (var-get platform-fee-rate)
)

;; Admin functions

(define-public (set-platform-fee-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (asserts! (<= new-rate u1000) ERR_INVALID_AMOUNT) ;; Max 10%
        (var-set platform-fee-rate new-rate)
        (ok true)
    )
)

(define-public (emergency-pause-project (project-id uint))
    (let ((project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (map-set projects project-id (merge project {status: "paused"}))
        (ok true)
    )
)