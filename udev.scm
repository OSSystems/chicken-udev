(module udev

(udev-monitor-start
 udev-monitor-stop!

 ;; udev-device record
 udev-device-node
 udev-device-type
 udev-device-subsystem
 udev-device-action
)

(import chicken scheme foreign)
(use extras srfi-18)

(foreign-declare "#include <libudev.h>")

(define %new-udev
  (foreign-lambda* (c-pointer (struct "udev")) ()
    "struct udev *udev;
     udev = udev_new();
     C_return(udev);
    "))

(define (new-udev)
  (or (%new-udev)
      (signal
       (make-composite-condition
        (make-property-condition 'udev)
        (make-property-condition
         'exn
         'location 'new-udev
         'message "Could not create udev context")))))

(define new-monitor
  (foreign-lambda* (c-pointer (struct "udev_monitor"))
                   (((c-pointer (struct "udev")) udev))
    "struct udev_monitor *mon;
     mon = udev_monitor_new_from_netlink(udev, \"udev\");
     udev_monitor_enable_receiving(mon);
     C_return(mon);
    "))

(define monitor->fd
  (foreign-lambda* int (((c-pointer (struct "udev_monitor")) mon))
    "C_return(udev_monitor_get_fd(mon));"))

(define %receive-device
  (foreign-lambda (c-pointer (struct "udev_device"))
                  udev_monitor_receive_device
                  (c-pointer (struct "udev_monitor"))))

(define unref-device
  (foreign-lambda void udev_device_unref (c-pointer (struct "udev_device"))))

(define-record udev-device node subsystem type action syspath sysname sysnum)

(define-record-printer (udev-device obj out)
  (fprintf out
           "#<device node: ~S subsystem: ~S type: ~S action: ~S syspath: ~S sysname: ~S sysnum: ~S>"
           (udev-device-node obj)
           (udev-device-subsystem obj)
           (udev-device-type obj)
           (udev-device-action obj)
           (udev-device-syspath obj)
           (udev-device-sysname obj)
           (udev-device-sysnum obj)))

(define-syntax udev-device-get-string
  (syntax-rules ()
    ((_ c-func)
     (foreign-lambda c-string c-func (c-pointer (struct "udev_device"))))))

(define (receive-device mon)
  (let ((dev (%receive-device mon)))
    (and dev
         (let ((udev-dev
                (make-udev-device
                 ((udev-device-get-string "udev_device_get_devnode") dev)
                 ((udev-device-get-string "udev_device_get_subsystem") dev)
                 ((udev-device-get-string "udev_device_get_devtype") dev)
                 ((udev-device-get-string "udev_device_get_action") dev)
                 ((udev-device-get-string "udev_device_get_syspath") dev)
                 ((udev-device-get-string "udev_device_get_sysname") dev)
                 ((udev-device-get-string "udev_device_get_sysnum") dev))))
           (unref-device dev)
           udev-dev))))

(define (udev-monitor-start dispatcher #!key (polling-interval 0.2))
  (let* ((udev (new-udev))
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
    ((foreign-lambda void "udev_unref" (c-pointer (struct "udev"))) udev)
    ((foreign-lambda void "udev_monitor_unref" (c-pointer (struct "udev_monitor"))) mon)
    (thread-terminate! monitor)
    ))

) ;; end module
