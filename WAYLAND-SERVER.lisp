(PROGN
 (PROGN
  (DEFCLASS wl_display () ((client :initarg :client :accessor client)))
  (DEFMETHOD evt-error ((OBJ wl_display) object_id code message))
  (DEFMETHOD evt-delete_id ((OBJ wl_display) id))
  (DEFMETHOD req-sync ((OBJ wl_display) callback))
  (DEFMETHOD req-get_registry ((OBJ wl_display) registry))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_display) OPCODE)
    (NTH OPCODE '(EVT-ERROR EVT-DELETE_ID))))
 (PROGN
  (DEFCLASS wl_registry () ((client :initarg :client :accessor client)))
  (DEFMETHOD evt-global ((OBJ wl_registry) name interface version))
  (DEFMETHOD evt-global_remove ((OBJ wl_registry) name))
  (DEFMETHOD req-bind ((OBJ wl_registry) name id))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_registry) OPCODE)
    (NTH OPCODE '(EVT-GLOBAL EVT-GLOBAL_REMOVE))))
 (PROGN
  (DEFCLASS wl_callback () ((client :initarg :client :accessor client)))
  (DEFMETHOD evt-done ((OBJ wl_callback) callback_data))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_callback) OPCODE)
    (NTH OPCODE '(EVT-DONE))))
 (PROGN
  (DEFCLASS wl_compositor () ((client :initarg :client :accessor client)))
  (DEFMETHOD req-create_surface ((OBJ wl_compositor) id))
  (DEFMETHOD req-create_region ((OBJ wl_compositor) id))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_compositor) OPCODE)
    (NTH OPCODE 'NIL)))
 (PROGN
  (DEFCLASS wl_shm_pool () ((client :initarg :client :accessor client)))
  (DEFMETHOD req-create_buffer
             ((OBJ wl_shm_pool) id offset width height stride format))
  (DEFMETHOD req-destroy ((OBJ wl_shm_pool)))
  (DEFMETHOD req-resize ((OBJ wl_shm_pool) size))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_shm_pool) OPCODE) (NTH OPCODE 'NIL)))
 (PROGN
  (DEFCLASS wl_shm () ((client :initarg :client :accessor client)))
  (DEFMETHOD evt-format ((OBJ wl_shm) format))
  (DEFMETHOD req-create_pool ((OBJ wl_shm) id fd size))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_shm) OPCODE)
    (NTH OPCODE '(EVT-FORMAT))))
 (PROGN
  (DEFCLASS wl_buffer () ((client :initarg :client :accessor client)))
  (DEFMETHOD evt-release ((OBJ wl_buffer)))
  (DEFMETHOD req-destroy ((OBJ wl_buffer)))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_buffer) OPCODE)
    (NTH OPCODE '(EVT-RELEASE))))
 (PROGN
  (DEFCLASS wl_data_offer () ((client :initarg :client :accessor client)))
  (DEFMETHOD evt-offer ((OBJ wl_data_offer) mime_type))
  (DEFMETHOD evt-source_actions ((OBJ wl_data_offer) source_actions))
  (DEFMETHOD evt-action ((OBJ wl_data_offer) dnd_action))
  (DEFMETHOD req-accept ((OBJ wl_data_offer) serial mime_type))
  (DEFMETHOD req-receive ((OBJ wl_data_offer) mime_type fd))
  (DEFMETHOD req-destroy ((OBJ wl_data_offer)))
  (DEFMETHOD req-finish ((OBJ wl_data_offer)))
  (DEFMETHOD req-set_actions
             ((OBJ wl_data_offer) dnd_actions preferred_action))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_data_offer) OPCODE)
    (NTH OPCODE '(EVT-OFFER EVT-SOURCE_ACTIONS EVT-ACTION))))
 (PROGN
  (DEFCLASS wl_data_source () ((client :initarg :client :accessor client)))
  (DEFMETHOD evt-target ((OBJ wl_data_source) mime_type))
  (DEFMETHOD evt-send ((OBJ wl_data_source) mime_type fd))
  (DEFMETHOD evt-cancelled ((OBJ wl_data_source)))
  (DEFMETHOD evt-dnd_drop_performed ((OBJ wl_data_source)))
  (DEFMETHOD evt-dnd_finished ((OBJ wl_data_source)))
  (DEFMETHOD evt-action ((OBJ wl_data_source) dnd_action))
  (DEFMETHOD req-offer ((OBJ wl_data_source) mime_type))
  (DEFMETHOD req-destroy ((OBJ wl_data_source)))
  (DEFMETHOD req-set_actions ((OBJ wl_data_source) dnd_actions))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_data_source) OPCODE)
    (NTH OPCODE
         '(EVT-TARGET EVT-SEND EVT-CANCELLED EVT-DND_DROP_PERFORMED
           EVT-DND_FINISHED EVT-ACTION))))
 (PROGN
  (DEFCLASS wl_data_device () ((client :initarg :client :accessor client)))
  (DEFMETHOD evt-data_offer ((OBJ wl_data_device) id))
  (DEFMETHOD evt-enter ((OBJ wl_data_device) serial surface x y id))
  (DEFMETHOD evt-leave ((OBJ wl_data_device)))
  (DEFMETHOD evt-motion ((OBJ wl_data_device) time x y))
  (DEFMETHOD evt-drop ((OBJ wl_data_device)))
  (DEFMETHOD evt-selection ((OBJ wl_data_device) id))
  (DEFMETHOD req-start_drag ((OBJ wl_data_device) source origin icon serial))
  (DEFMETHOD req-set_selection ((OBJ wl_data_device) source serial))
  (DEFMETHOD req-release ((OBJ wl_data_device)))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_data_device) OPCODE)
    (NTH OPCODE
         '(EVT-DATA_OFFER EVT-ENTER EVT-LEAVE EVT-MOTION EVT-DROP
           EVT-SELECTION))))
 (PROGN
  (DEFCLASS wl_data_device_manager ()
            ((client :initarg :client :accessor client)))
  (DEFMETHOD req-create_data_source ((OBJ wl_data_device_manager) id))
  (DEFMETHOD req-get_data_device ((OBJ wl_data_device_manager) id seat))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_data_device_manager) OPCODE)
    (NTH OPCODE 'NIL)))
 (PROGN
  (DEFCLASS wl_shell () ((client :initarg :client :accessor client)))
  (DEFMETHOD req-get_shell_surface ((OBJ wl_shell) id surface))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_shell) OPCODE) (NTH OPCODE 'NIL)))
 (PROGN
  (DEFCLASS wl_shell_surface () ((client :initarg :client :accessor client)))
  (DEFMETHOD evt-ping ((OBJ wl_shell_surface) serial))
  (DEFMETHOD evt-configure ((OBJ wl_shell_surface) edges width height))
  (DEFMETHOD evt-popup_done ((OBJ wl_shell_surface)))
  (DEFMETHOD req-pong ((OBJ wl_shell_surface) serial))
  (DEFMETHOD req-move ((OBJ wl_shell_surface) seat serial))
  (DEFMETHOD req-resize ((OBJ wl_shell_surface) seat serial edges))
  (DEFMETHOD req-set_toplevel ((OBJ wl_shell_surface)))
  (DEFMETHOD req-set_transient ((OBJ wl_shell_surface) parent x y flags))
  (DEFMETHOD req-set_fullscreen
             ((OBJ wl_shell_surface) method framerate output))
  (DEFMETHOD req-set_popup
             ((OBJ wl_shell_surface) seat serial parent x y flags))
  (DEFMETHOD req-set_maximized ((OBJ wl_shell_surface) output))
  (DEFMETHOD req-set_title ((OBJ wl_shell_surface) title))
  (DEFMETHOD req-set_class ((OBJ wl_shell_surface) class_))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_shell_surface) OPCODE)
    (NTH OPCODE '(EVT-PING EVT-CONFIGURE EVT-POPUP_DONE))))
 (PROGN
  (DEFCLASS wl_surface () ((client :initarg :client :accessor client)))
  (DEFMETHOD evt-enter ((OBJ wl_surface) output))
  (DEFMETHOD evt-leave ((OBJ wl_surface) output))
  (DEFMETHOD evt-preferred_buffer_scale ((OBJ wl_surface) factor))
  (DEFMETHOD evt-preferred_buffer_transform ((OBJ wl_surface) transform))
  (DEFMETHOD req-destroy ((OBJ wl_surface)))
  (DEFMETHOD req-attach ((OBJ wl_surface) buffer x y))
  (DEFMETHOD req-damage ((OBJ wl_surface) x y width height))
  (DEFMETHOD req-frame ((OBJ wl_surface) callback))
  (DEFMETHOD req-set_opaque_region ((OBJ wl_surface) region))
  (DEFMETHOD req-set_input_region ((OBJ wl_surface) region))
  (DEFMETHOD req-commit ((OBJ wl_surface)))
  (DEFMETHOD req-set_buffer_transform ((OBJ wl_surface) transform))
  (DEFMETHOD req-set_buffer_scale ((OBJ wl_surface) scale))
  (DEFMETHOD req-damage_buffer ((OBJ wl_surface) x y width height))
  (DEFMETHOD req-offset ((OBJ wl_surface) x y))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_surface) OPCODE)
    (NTH OPCODE
         '(EVT-ENTER EVT-LEAVE EVT-PREFERRED_BUFFER_SCALE
           EVT-PREFERRED_BUFFER_TRANSFORM))))
 (PROGN
  (DEFCLASS wl_seat () ((client :initarg :client :accessor client)))
  (DEFMETHOD evt-capabilities ((OBJ wl_seat) capabilities))
  (DEFMETHOD evt-name ((OBJ wl_seat) name))
  (DEFMETHOD req-get_pointer ((OBJ wl_seat) id))
  (DEFMETHOD req-get_keyboard ((OBJ wl_seat) id))
  (DEFMETHOD req-get_touch ((OBJ wl_seat) id))
  (DEFMETHOD req-release ((OBJ wl_seat)))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_seat) OPCODE)
    (NTH OPCODE '(EVT-CAPABILITIES EVT-NAME))))
 (PROGN
  (DEFCLASS wl_pointer () ((client :initarg :client :accessor client)))
  (DEFMETHOD evt-enter ((OBJ wl_pointer) serial surface surface_x surface_y))
  (DEFMETHOD evt-leave ((OBJ wl_pointer) serial surface))
  (DEFMETHOD evt-motion ((OBJ wl_pointer) time surface_x surface_y))
  (DEFMETHOD evt-button ((OBJ wl_pointer) serial time button state))
  (DEFMETHOD evt-axis ((OBJ wl_pointer) time axis value))
  (DEFMETHOD evt-frame ((OBJ wl_pointer)))
  (DEFMETHOD evt-axis_source ((OBJ wl_pointer) axis_source))
  (DEFMETHOD evt-axis_stop ((OBJ wl_pointer) time axis))
  (DEFMETHOD evt-axis_discrete ((OBJ wl_pointer) axis discrete))
  (DEFMETHOD evt-axis_value120 ((OBJ wl_pointer) axis value120))
  (DEFMETHOD evt-axis_relative_direction ((OBJ wl_pointer) axis direction))
  (DEFMETHOD req-set_cursor
             ((OBJ wl_pointer) serial surface hotspot_x hotspot_y))
  (DEFMETHOD req-release ((OBJ wl_pointer)))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_pointer) OPCODE)
    (NTH OPCODE
         '(EVT-ENTER EVT-LEAVE EVT-MOTION EVT-BUTTON EVT-AXIS EVT-FRAME
           EVT-AXIS_SOURCE EVT-AXIS_STOP EVT-AXIS_DISCRETE EVT-AXIS_VALUE120
           EVT-AXIS_RELATIVE_DIRECTION))))
 (PROGN
  (DEFCLASS wl_keyboard () ((client :initarg :client :accessor client)))
  (DEFMETHOD evt-keymap ((OBJ wl_keyboard) format fd size))
  (DEFMETHOD evt-enter ((OBJ wl_keyboard) serial surface keys))
  (DEFMETHOD evt-leave ((OBJ wl_keyboard) serial surface))
  (DEFMETHOD evt-key ((OBJ wl_keyboard) serial time key state))
  (DEFMETHOD evt-modifiers
             ((OBJ wl_keyboard) serial mods_depressed mods_latched mods_locked
              group))
  (DEFMETHOD evt-repeat_info ((OBJ wl_keyboard) rate delay))
  (DEFMETHOD req-release ((OBJ wl_keyboard)))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_keyboard) OPCODE)
    (NTH OPCODE
         '(EVT-KEYMAP EVT-ENTER EVT-LEAVE EVT-KEY EVT-MODIFIERS
           EVT-REPEAT_INFO))))
 (PROGN
  (DEFCLASS wl_touch () ((client :initarg :client :accessor client)))
  (DEFMETHOD evt-down ((OBJ wl_touch) serial time surface id x y))
  (DEFMETHOD evt-up ((OBJ wl_touch) serial time id))
  (DEFMETHOD evt-motion ((OBJ wl_touch) time id x y))
  (DEFMETHOD evt-frame ((OBJ wl_touch)))
  (DEFMETHOD evt-cancel ((OBJ wl_touch)))
  (DEFMETHOD evt-shape ((OBJ wl_touch) id major minor))
  (DEFMETHOD evt-orientation ((OBJ wl_touch) id orientation))
  (DEFMETHOD req-release ((OBJ wl_touch)))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_touch) OPCODE)
    (NTH OPCODE
         '(EVT-DOWN EVT-UP EVT-MOTION EVT-FRAME EVT-CANCEL EVT-SHAPE
           EVT-ORIENTATION))))
 (PROGN
  (DEFCLASS wl_output () ((client :initarg :client :accessor client)))
  (DEFMETHOD evt-geometry
             ((OBJ wl_output) x y physical_width physical_height subpixel make
              model transform))
  (DEFMETHOD evt-mode ((OBJ wl_output) flags width height refresh))
  (DEFMETHOD evt-done ((OBJ wl_output)))
  (DEFMETHOD evt-scale ((OBJ wl_output) factor))
  (DEFMETHOD evt-name ((OBJ wl_output) name))
  (DEFMETHOD evt-description ((OBJ wl_output) description))
  (DEFMETHOD req-release ((OBJ wl_output)))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_output) OPCODE)
    (NTH OPCODE
         '(EVT-GEOMETRY EVT-MODE EVT-DONE EVT-SCALE EVT-NAME
           EVT-DESCRIPTION))))
 (PROGN
  (DEFCLASS wl_region () ((client :initarg :client :accessor client)))
  (DEFMETHOD req-destroy ((OBJ wl_region)))
  (DEFMETHOD req-add ((OBJ wl_region) x y width height))
  (DEFMETHOD req-subtract ((OBJ wl_region) x y width height))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_region) OPCODE) (NTH OPCODE 'NIL)))
 (PROGN
  (DEFCLASS wl_subcompositor () ((client :initarg :client :accessor client)))
  (DEFMETHOD req-destroy ((OBJ wl_subcompositor)))
  (DEFMETHOD req-get_subsurface ((OBJ wl_subcompositor) id surface parent))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_subcompositor) OPCODE)
    (NTH OPCODE 'NIL)))
 (PROGN
  (DEFCLASS wl_subsurface () ((client :initarg :client :accessor client)))
  (DEFMETHOD req-destroy ((OBJ wl_subsurface)))
  (DEFMETHOD req-set_position ((OBJ wl_subsurface) x y))
  (DEFMETHOD req-place_above ((OBJ wl_subsurface) sibling))
  (DEFMETHOD req-place_below ((OBJ wl_subsurface) sibling))
  (DEFMETHOD req-set_sync ((OBJ wl_subsurface)))
  (DEFMETHOD req-set_desync ((OBJ wl_subsurface)))
  (DEFMETHOD MATCH-EVENT-OPCODE ((OBJ wl_subsurface) OPCODE)
    (NTH OPCODE 'NIL))))