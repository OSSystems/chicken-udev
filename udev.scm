(module udev

(udev-monitor-start
 udev-monitor-stop!
 udev-list-devices
 udev-device-parent-with-subsystem-devtype

 ;; udev-device record
 udev-device?
 udev-device-node
 udev-device-type
 udev-device-subsystem
 udev-device-action
 udev-device-syspath
 udev-device-sysname
 udev-device-sysnum
 udev-device-driver
 udev-device-properties
)

(import chicken scheme foreign)
(use extras srfi-1 srfi-18)

(foreign-declare "#include <libudev.h>")

(define %udev-new
  (foreign-lambda* (c-pointer (struct "udev")) ()
    "struct udev *udev;
     udev = udev_new();
     C_return(udev);
    "))

(define (udev-new)
  (or (%udev-new)
      (signal
       (make-composite-condition
        (make-property-condition 'udev)
        (make-property-condition
         'exn
         'location 'udev-new
         'message "Could not create udev context")))))

(define %udev-unref
  (foreign-lambda void "udev_unref" (c-pointer (struct "udev"))))

(define new-monitor
  (foreign-lambda* (c-pointer (struct "udev_monitor"))
                   (((c-pointer (struct "udev")) udev))
    "struct udev_monitor *mon;
     mon = udev_monitor_new_from_netlink(udev, \"udev\");
     udev_monitor_enable_receiving(mon);
     C_return(mon);
    "))

(define %udev-monitor-unref
  (foreign-lambda void
                  "udev_monitor_unref"
                  (c-pointer (struct "udev_monitor"))))

(define monitor->fd
  (foreign-lambda* int (((c-pointer (struct "udev_monitor")) mon))
    "C_return(udev_monitor_get_fd(mon));"))

(define %receive-device
  (foreign-lambda (c-pointer (struct "udev_device"))
                  udev_monitor_receive_device
                  (c-pointer (struct "udev_monitor"))))

(define %udev-device-unref
  (foreign-lambda void "udev_device_unref" (c-pointer (struct "udev_device"))))

(define-record udev-device
  node
  subsystem
  type
  action
  syspath
  sysname
  sysnum
  driver
  properties)

(define %make-udev-device make-udev-device)

(define-syntax udev-device-get-string
  (syntax-rules ()
    ((_ c-func)
     (foreign-lambda c-string c-func (c-pointer (struct "udev_device"))))))


(define %udev-device-get-properties-list-entry
  (foreign-lambda (c-pointer (struct "udev_list_entry"))
                  "udev_device_get_properties_list_entry"
                  (c-pointer (struct "udev_device"))))


(define (udev-device-get-properties dev)
  (let loop ((entry (%udev-device-get-properties-list-entry dev)))
    (if entry
        (let* ((key (%udev-list-get-entry-name entry))
               (val (%udev-list-get-entry-value entry)))
          (cons
           (cons key val)
           (loop (%udev-list-entry-get-next entry))))
        '())))

(define (make-udev-device dev)
  (%make-udev-device
   ((udev-device-get-string "udev_device_get_devnode") dev)
   ((udev-device-get-string "udev_device_get_subsystem") dev)
   ((udev-device-get-string "udev_device_get_devtype") dev)
   ((udev-device-get-string "udev_device_get_action") dev)
   ((udev-device-get-string "udev_device_get_syspath") dev)
   ((udev-device-get-string "udev_device_get_sysname") dev)
   ((udev-device-get-string "udev_device_get_sysnum") dev)
   ((udev-device-get-string "udev_device_get_driver") dev)
   (udev-device-get-properties dev)))

(define-record-printer (udev-device obj out)
  (fprintf out
           "#<device node: ~S subsystem: ~S type: ~S action: ~S syspath: ~S sysname: ~S sysnum: ~S driver: ~S properties: ~S>"
           (udev-device-node obj)
           (udev-device-subsystem obj)
           (udev-device-type obj)
           (udev-device-action obj)
           (udev-device-syspath obj)
           (udev-device-sysname obj)
           (udev-device-sysnum obj)
           (udev-device-driver obj)
           (udev-device-properties obj)))

(define %udev-enumerate-new
  (foreign-lambda (c-pointer (struct "udev_enumerate"))
                  "udev_enumerate_new"
                  (c-pointer (struct "udev"))))

(define %udev-enumerate-unref
  (foreign-lambda void
                  "udev_enumerate_unref"
                  (c-pointer (struct "udev_enumerate"))))

(define %udev-enumerate-scan-devices
  (foreign-lambda int
                  "udev_enumerate_scan_devices"
                  (c-pointer (struct "udev_enumerate"))))

(define %udev-enumerate-get-list-entry
  (foreign-lambda (c-pointer (struct "udev_list_entry"))
                  "udev_enumerate_get_list_entry"
                  (c-pointer (struct "udev_enumerate"))))

(define %udev-list-entry-get-next
  (foreign-lambda (c-pointer (struct "udev_list_entry"))
                  "udev_list_entry_get_next"
                  (c-pointer (struct "udev_list_entry"))))

(define %udev-list-get-entry-name
  (foreign-lambda c-string
                  "udev_list_entry_get_name"
                  (c-pointer (struct "udev_list_entry"))))

(define %udev-list-get-entry-value
  (foreign-lambda c-string
                  "udev_list_entry_get_value"
                  (c-pointer (struct "udev_list_entry"))))

(define %udev-list-entry-get-value
  (foreign-lambda c-string
                  "udev_list_entry_get_value"
                  (c-pointer (struct "udev_list_entry"))))

(define %udev-device-new-from-syspath
  (foreign-lambda (c-pointer (struct "udev_device"))
                  "udev_device_new_from_syspath"
                  (c-pointer (struct "udev"))
                  c-string))

(define %udev-device-get-parent-with-subsystem-devtype
  (foreign-lambda (c-pointer (struct "udev_device"))
                  "udev_device_get_parent_with_subsystem_devtype"
                  (c-pointer (struct "udev_device"))
                  c-string
                  c-string))

(define (udev-scan-devices udev uenum)
  (if (zero? (%udev-enumerate-scan-devices uenum))
      (void)
      (signal
       (make-composite-condition
        (make-property-condition 'udev)
        (make-property-condition
         'exn
         'location '%udev-scan-devices
         'message "Could not scan devices")))))


;;;
;;; Monitoring devices
;;;

(define (receive-device mon)
  (let ((dev (%receive-device mon)))
    (and dev
         (let ((udev-dev (make-udev-device dev)))
           (%udev-device-unref dev)
           udev-dev))))

(define (udev-monitor-start dispatcher #!key (polling-interval 0.2))
  (let* ((udev (udev-new))
         (monitor (new-monitor udev))
         (monitor-fd (monitor->fd monitor))
         (thread
          (make-thread
           (lambda ()
             (let loop ()
               (let ((dev (receive-device monitor)))
                 (if dev
                     (dispatcher dev)
                     (thread-sleep! polling-interval))
                 (loop)))))))
    (thread-specific-set! thread (cons udev monitor))
    (thread-start! thread)))

(define (udev-monitor-stop! monitor)
  (let* ((mon-data (thread-specific monitor))
         (udev (car mon-data))
         (mon (cdr mon-data)))
    (%udev-unref udev)
    (%udev-monitor-unref mon)
    (thread-terminate! monitor)))


;;;
;;; Listing devices
;;;

(define (udev-list-devices)
  (let* ((udev (udev-new))
         (uenum (%udev-enumerate-new udev)))
    (udev-scan-devices udev uenum)
    (let ((paths
           (let loop ((entry (%udev-enumerate-get-list-entry uenum)))
             (if entry
                 (let ((path (%udev-list-get-entry-name entry)))
                   (cons path
                         (loop (%udev-list-entry-get-next entry))))
                 '()))))
      (%udev-enumerate-unref uenum)
      (%udev-unref udev)
      (filter-map
       (lambda (path)
         (and-let* ((udev (handle-exceptions exn
                            #f
                            (udev-new)))
                    (raw-dev (%udev-device-new-from-syspath udev path))
                    (dev (make-udev-device raw-dev)))
           (%udev-device-unref raw-dev)
           (%udev-unref udev)
           dev))
       paths))))


;;;
;;; Parenthood
;;;
(define (udev-device-parent-with-subsystem-devtype dev subsystem devtype)
  (and-let* ((udev (udev-new))
             (path (udev-device-syspath dev))
             (dev (%udev-device-new-from-syspath udev path))
             (parent
              (%udev-device-get-parent-with-subsystem-devtype dev
                                                              subsystem
                                                              devtype)))
    (%udev-unref udev)
    (let ((parent-dev (make-udev-device parent)))
      (%udev-device-unref parent)
      parent-dev)))

) ;; end module
