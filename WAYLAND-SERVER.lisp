(DEFPACKAGE :WL
  (:USE #:CL)
  (:EXPORT WL-OBJECT
           *OBJECTS*
           MATCH-EVENT-OPCODE
           MATCH-REQUEST-OPCODE
           GET-REQUEST-ARG-TYPES
           INT
           UINT
           OBJECT
           NEW_ID
           FIXED
           STRING
           ARRAY
           FD
           ENUM))

(IN-PACKAGE :WL)

(DEFVAR *ARG-TYPE-SYMBOLS* '(INT UINT OBJECT NEW_ID FIXED STRING ARRAY FD ENUM))

(DEFVAR *OBJECTS* (MAKE-HASH-TABLE :TEST 'EQ))

(DEFCLASS WL-OBJECT NIL ((ID :INITARG :ID :ACCESSOR ID)))

(DEFGENERIC MATCH-EVENT-OPCODE
    (OBJ OPCODE))

(DEFGENERIC MATCH-REQUEST-OPCODE
    (OBJ OPCODE))

(DEFGENERIC GET-REQUEST-ARG-TYPES
    (OBJ OPCODE))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((OBJ WL-OBJECT) &KEY)
  (SETF (GETHASH (ID OBJ) *OBJECTS*) OBJ))

(DEFPACKAGE :WL/WL_DISPLAY
  (:USE :CL :WL)
  (:EXPORT WL_DISPLAY REQ-SYNC REQ-GET_REGISTRY EVT-ERROR EVT-DELETE_ID))

(IN-PACKAGE :WL/WL_DISPLAY)

(DEFCLASS WL_DISPLAY (WL-OBJECT) ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "core global object

The core global object.  This is a special singleton object.  It
      is used for internal Wayland protocol features.
"))

(DEFMETHOD EVT-ERROR ((OBJ WL_DISPLAY) STREAM OBJECT_ID CODE MESSAGE)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-ERROR)))
    ";; fatal error event

The error event is sent out when a fatal (non-recoverable)
	error has occurred.  The object_id argument is the object
	where the error occurred, most often in response to a request
	to that object.  The code identifies the error and is defined
	by the object interface.  As such, each interface defines its
	own set of error codes.  The message is a brief description
	of the error, for (debugging) convenience.

Arguments:
object_id::object: object where the error occurred
code::uint: error code
message::string: error description
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-DELETE_ID ((OBJ WL_DISPLAY) STREAM ID)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-DELETE_ID)))
    ";; acknowledge object ID deletion

This event is used internally by the object ID management
	logic. When a client deletes an object that it had created,
	the server will send this event to acknowledge that it has
	seen the delete request. When the client receives this event,
	it will know that it can safely reuse the object ID.

Arguments:
id::uint: deleted object ID
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD REQ-SYNC ((OBJ WL_DISPLAY) CLIENT CALLBACK)
  ";; asynchronous roundtrip

The sync request asks the server to emit the 'done' event
	on the returned wl_callback object.  Since requests are
	handled in-order and events are delivered in-order, this can
	be used as a barrier to ensure all previous requests and the
	resulting events have been handled.

	The object returned by this request will be destroyed by the
	compositor after the callback is fired and as such the client must not
	attempt to use it after that point.

	The callback_data passed in the callback is the event serial.

Arguments:
callback::new_id: callback object for the sync request
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-GET_REGISTRY ((OBJ WL_DISPLAY) CLIENT REGISTRY)
  ";; get global registry object

This request creates a registry object that allows the client
	to list and bind the global objects available from the
	compositor.

	It should be noted that the server side resources consumed in
	response to a get_registry request can only be released when the
	client disconnects, not when the client side proxy is destroyed.
	Therefore, clients should invoke get_registry as infrequently as
	possible to avoid wasting memory.

Arguments:
registry::new_id: global registry object
"
  (ERROR "Unimplemented"))

(DEFMETHOD ENUM-ERROR ((OBJ WL_DISPLAY) VALUE)
  ";; global error values

These errors are global and can be emitted in response to any
	server request.
"
  (CASE VALUE
    (0 'INVALID_OBJECT)
    (1 'INVALID_METHOD)
    (2 'NO_MEMORY)
    (3 'IMPLEMENTATION)))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_DISPLAY) EVENT)
  (CASE EVENT (EVT-ERROR 0) (EVT-DELETE_ID 1)))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_DISPLAY) OPCODE)
  (NTH OPCODE '(REQ-SYNC REQ-GET_REGISTRY)))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_DISPLAY) OPCODE)
  (NTH OPCODE '((NEW_ID) (NEW_ID))))

(DEFPACKAGE :WL/WL_REGISTRY
  (:USE :CL :WL)
  (:EXPORT WL_REGISTRY REQ-BIND EVT-GLOBAL EVT-GLOBAL_REMOVE))

(IN-PACKAGE :WL/WL_REGISTRY)

(DEFCLASS WL_REGISTRY (WL-OBJECT) ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "global registry object

The singleton global registry object.  The server has a number of
      global objects that are available to all clients.  These objects
      typically represent an actual object in the server (for example,
      an input device) or they are singleton objects that provide
      extension functionality.

      When a client creates a registry object, the registry object
      will emit a global event for each global currently in the
      registry.  Globals come and go as a result of device or
      monitor hotplugs, reconfiguration or other events, and the
      registry will send out global and global_remove events to
      keep the client up to date with the changes.  To mark the end
      of the initial burst of events, the client can use the
      wl_display.sync request immediately after calling
      wl_display.get_registry.

      A client can bind to a global object by using the bind
      request.  This creates a client-side handle that lets the object
      emit events to the client and lets the client invoke requests on
      the object.
"))

(DEFMETHOD EVT-GLOBAL ((OBJ WL_REGISTRY) STREAM NAME INTERFACE VERSION)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-GLOBAL)))
    ";; announce global object

Notify the client of global objects.

	The event notifies the client that a global object with
	the given name is now available, and it implements the
	given version of the given interface.

Arguments:
name::uint: numeric name of the global object
interface::string: interface implemented by the object
version::uint: interface version
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-GLOBAL_REMOVE ((OBJ WL_REGISTRY) STREAM NAME)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-GLOBAL_REMOVE)))
    ";; announce removal of global object

Notify the client of removed global objects.

	This event notifies the client that the global identified
	by name is no longer available.  If the client bound to
	the global using the bind request, the client should now
	destroy that object.

	The object remains valid and requests to the object will be
	ignored until the client destroys it, to avoid races between
	the global going away and a client sending a request to it.

Arguments:
name::uint: numeric name of the global object
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD REQ-BIND ((OBJ WL_REGISTRY) CLIENT NAME ID)
  ";; bind an object to the display

Binds a new, client-created object to the server using the
	specified name as the identifier.

Arguments:
name::uint: unique numeric name of the object
id::new_id: bounded object
"
  (ERROR "Unimplemented"))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_REGISTRY) EVENT)
  (CASE EVENT (EVT-GLOBAL 0) (EVT-GLOBAL_REMOVE 1)))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_REGISTRY) OPCODE)
  (NTH OPCODE '(REQ-BIND)))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_REGISTRY) OPCODE)
  (NTH OPCODE '((UINT NEW_ID))))

(DEFPACKAGE :WL/WL_CALLBACK
  (:USE :CL :WL)
  (:EXPORT WL_CALLBACK EVT-DONE))

(IN-PACKAGE :WL/WL_CALLBACK)

(DEFCLASS WL_CALLBACK (WL-OBJECT) ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "callback object

Clients can handle the 'done' event to get notified when
      the related request is done.

      Note, because wl_callback objects are created from multiple independent
      factory interfaces, the wl_callback interface is frozen at version 1.
"))

(DEFMETHOD EVT-DONE ((OBJ WL_CALLBACK) STREAM CALLBACK_DATA)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-DONE)))
    ";; done event

Notify the client when the related request is done.

Arguments:
callback_data::uint: request-specific data for the callback
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_CALLBACK) EVENT)
  (CASE EVENT (EVT-DONE 0) (t (error "Unknown event opcode"))))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_CALLBACK) OPCODE) (NTH OPCODE 'NIL))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_CALLBACK) OPCODE) (NTH OPCODE 'NIL))

(DEFPACKAGE :WL/WL_COMPOSITOR
  (:USE :CL :WL)
  (:EXPORT WL_COMPOSITOR REQ-CREATE_SURFACE REQ-CREATE_REGION))

(IN-PACKAGE :WL/WL_COMPOSITOR)

(DEFCLASS WL_COMPOSITOR (WL-OBJECT)
          ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "the compositor singleton

A compositor.  This object is a singleton global.  The
      compositor is in charge of combining the contents of multiple
      surfaces into one displayable output.
"))

(DEFMETHOD REQ-CREATE_SURFACE ((OBJ WL_COMPOSITOR) CLIENT ID)
  ";; create new surface

Ask the compositor to create a new surface.

Arguments:
id::new_id: the new surface
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-CREATE_REGION ((OBJ WL_COMPOSITOR) CLIENT ID)
  ";; create new region

Ask the compositor to create a new region.

Arguments:
id::new_id: the new region
"
  (ERROR "Unimplemented"))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_COMPOSITOR) EVENT) (CASE EVENT))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_COMPOSITOR) OPCODE)
  (NTH OPCODE '(REQ-CREATE_SURFACE REQ-CREATE_REGION)))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_COMPOSITOR) OPCODE)
  (NTH OPCODE '((NEW_ID) (NEW_ID))))

(DEFPACKAGE :WL/WL_SHM_POOL
  (:USE :CL :WL)
  (:EXPORT WL_SHM_POOL REQ-CREATE_BUFFER REQ-DESTROY REQ-RESIZE))

(IN-PACKAGE :WL/WL_SHM_POOL)

(DEFCLASS WL_SHM_POOL (WL-OBJECT) ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "a shared memory pool

The wl_shm_pool object encapsulates a piece of memory shared
      between the compositor and client.  Through the wl_shm_pool
      object, the client can allocate shared memory wl_buffer objects.
      All objects created through the same pool share the same
      underlying mapped memory. Reusing the mapped memory avoids the
      setup/teardown overhead and is useful when interactively resizing
      a surface or for many small buffers.
"))

(DEFMETHOD REQ-CREATE_BUFFER
           ((OBJ WL_SHM_POOL) CLIENT ID OFFSET WIDTH HEIGHT STRIDE FORMAT)
  ";; create a buffer from the pool

Create a wl_buffer object from the pool.

	The buffer is created offset bytes into the pool and has
	width and height as specified.  The stride argument specifies
	the number of bytes from the beginning of one row to the beginning
	of the next.  The format is the pixel format of the buffer and
	must be one of those advertised through the wl_shm.format event.

	A buffer will keep a reference to the pool it was created from
	so it is valid to destroy the pool immediately after creating
	a buffer from it.

Arguments:
id::new_id: buffer to create
offset::int: buffer byte offset within the pool
width::int: buffer width, in pixels
height::int: buffer height, in pixels
stride::int: number of bytes from the beginning of one row to the beginning of the next row
format::enum: buffer pixel format
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-DESTROY ((OBJ WL_SHM_POOL) CLIENT)
  ";; destroy the pool

Destroy the shared memory pool.

	The mmapped memory will be released when all
	buffers that have been created from this pool
	are gone.
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-RESIZE ((OBJ WL_SHM_POOL) CLIENT SIZE)
  ";; change the size of the pool mapping

This request will cause the server to remap the backing memory
	for the pool from the file descriptor passed when the pool was
	created, but using the new size.  This request can only be
	used to make the pool bigger.

        This request only changes the amount of bytes that are mmapped
        by the server and does not touch the file corresponding to the
        file descriptor passed at creation time. It is the client's
        responsibility to ensure that the file is at least as big as
        the new pool size.

Arguments:
size::int: new size of the pool, in bytes
"
  (ERROR "Unimplemented"))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_SHM_POOL) EVENT) (CASE EVENT))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_SHM_POOL) OPCODE)
  (NTH OPCODE '(REQ-CREATE_BUFFER REQ-DESTROY REQ-RESIZE)))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_SHM_POOL) OPCODE)
  (NTH OPCODE '((NEW_ID INT INT INT INT (ENUM (WL_SHM FORMAT))) NIL (INT))))

(DEFPACKAGE :WL/WL_SHM
  (:USE :CL :WL)
  (:EXPORT WL_SHM REQ-CREATE_POOL EVT-FORMAT))

(IN-PACKAGE :WL/WL_SHM)

(DEFCLASS WL_SHM (WL-OBJECT) ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "shared memory support

A singleton global object that provides support for shared
      memory.

      Clients can create wl_shm_pool objects using the create_pool
      request.

      On binding the wl_shm object one or more format events
      are emitted to inform clients about the valid pixel formats
      that can be used for buffers.
"))

(DEFMETHOD EVT-FORMAT ((OBJ WL_SHM) STREAM FORMAT)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-FORMAT)))
    ";; pixel format description

Informs the client about a valid pixel format that
	can be used for buffers. Known formats include
	argb8888 and xrgb8888.

Arguments:
format::enum: buffer pixel format
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD REQ-CREATE_POOL ((OBJ WL_SHM) CLIENT ID FD SIZE)
  ";; create a shm pool

Create a new wl_shm_pool object.

	The pool can be used to create shared memory based buffer
	objects.  The server will mmap size bytes of the passed file
	descriptor, to use as backing memory for the pool.

Arguments:
id::new_id: pool to create
fd::fd: file descriptor for the pool
size::int: pool size, in bytes
"
  (ERROR "Unimplemented"))

(DEFMETHOD ENUM-ERROR ((OBJ WL_SHM) VALUE)
  ";; wl_shm error values

These errors can be emitted in response to wl_shm requests.
"
  (CASE VALUE (0 'INVALID_FORMAT) (1 'INVALID_STRIDE) (2 'INVALID_FD)))

(DEFMETHOD ENUM-FORMAT ((OBJ WL_SHM) VALUE)
  ";; pixel formats

This describes the memory layout of an individual pixel.

	All renderers should support argb8888 and xrgb8888 but any other
	formats are optional and may not be supported by the particular
	renderer in use.

	The drm format codes match the macros defined in drm_fourcc.h, except
	argb8888 and xrgb8888. The formats actually supported by the compositor
	will be reported by the format event.

	For all wl_shm formats and unless specified in another protocol
	extension, pre-multiplied alpha is used for pixel values.
"
  (CASE VALUE
    (0 'ARGB8888)
    (1 'XRGB8888)
    (538982467 'C8)
    (943867730 'RGB332)
    (944916290 'BGR233)
    (842093144 'XRGB4444)
    (842089048 'XBGR4444)
    (842094674 'RGBX4444)
    (842094658 'BGRX4444)
    (842093121 'ARGB4444)
    (842089025 'ABGR4444)
    (842088786 'RGBA4444)
    (842088770 'BGRA4444)
    (892424792 'XRGB1555)
    (892420696 'XBGR1555)
    (892426322 'RGBX5551)
    (892426306 'BGRX5551)
    (892424769 'ARGB1555)
    (892420673 'ABGR1555)
    (892420434 'RGBA5551)
    (892420418 'BGRA5551)
    (909199186 'RGB565)
    (909199170 'BGR565)
    (875710290 'RGB888)
    (875710274 'BGR888)
    (875709016 'XBGR8888)
    (875714642 'RGBX8888)
    (875714626 'BGRX8888)
    (875708993 'ABGR8888)
    (875708754 'RGBA8888)
    (875708738 'BGRA8888)
    (808669784 'XRGB2101010)
    (808665688 'XBGR2101010)
    (808671314 'RGBX1010102)
    (808671298 'BGRX1010102)
    (808669761 'ARGB2101010)
    (808665665 'ABGR2101010)
    (808665426 'RGBA1010102)
    (808665410 'BGRA1010102)
    (1448695129 'YUYV)
    (1431918169 'YVYU)
    (1498831189 'UYVY)
    (1498765654 'VYUY)
    (1448433985 'AYUV)
    (842094158 'NV12)
    (825382478 'NV21)
    (909203022 'NV16)
    (825644622 'NV61)
    (961959257 'YUV410)
    (961893977 'YVU410)
    (825316697 'YUV411)
    (825316953 'YVU411)
    (842093913 'YUV420)
    (842094169 'YVU420)
    (909202777 'YUV422)
    (909203033 'YVU422)
    (875713881 'YUV444)
    (875714137 'YVU444)
    (538982482 'R8)
    (540422482 'R16)
    (943212370 'RG88)
    (943215175 'GR88)
    (842221394 'RG1616)
    (842224199 'GR1616)
    (1211388504 'XRGB16161616F)
    (1211384408 'XBGR16161616F)
    (1211388481 'ARGB16161616F)
    (1211384385 'ABGR16161616F)
    (1448434008 'XYUV8888)
    (875713878 'VUY888)
    (808670550 'VUY101010)
    (808530521 'Y210)
    (842084953 'Y212)
    (909193817 'Y216)
    (808531033 'Y410)
    (842085465 'Y412)
    (909194329 'Y416)
    (808670808 'XVYU2101010)
    (909334104 'XVYU12_16161616)
    (942954072 'XVYU16161616)
    (810299481 'Y0L0)
    (810299480 'X0L0)
    (843853913 'Y0L2)
    (843853912 'X0L2)
    (942691673 'YUV420_8BIT)
    (808539481 'YUV420_10BIT)
    (943805016 'XRGB8888_A8)
    (943800920 'XBGR8888_A8)
    (943806546 'RGBX8888_A8)
    (943806530 'BGRX8888_A8)
    (943798354 'RGB888_A8)
    (943798338 'BGR888_A8)
    (943797586 'RGB565_A8)
    (943797570 'BGR565_A8)
    (875714126 'NV24)
    (842290766 'NV42)
    (808530512 'P210)
    (808530000 'P010)
    (842084432 'P012)
    (909193296 'P016)
    (808534593 'AXBXGXRX106106106106)
    (892425806 'NV15)
    (808531025 'Q410)
    (825242705 'Q401)
    (942953048 'XRGB16161616)
    (942948952 'XBGR16161616)
    (942953025 'ARGB16161616)
    (942948929 'ABGR16161616)))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_SHM) EVENT) (CASE EVENT (EVT-FORMAT 0)))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_SHM) OPCODE)
  (NTH OPCODE '(REQ-CREATE_POOL)))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_SHM) OPCODE)
  (NTH OPCODE '((NEW_ID FD INT))))

(DEFPACKAGE :WL/WL_BUFFER
  (:USE :CL :WL)
  (:EXPORT WL_BUFFER REQ-DESTROY EVT-RELEASE))

(IN-PACKAGE :WL/WL_BUFFER)

(DEFCLASS WL_BUFFER (WL-OBJECT) ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "content for a wl_surface

A buffer provides the content for a wl_surface. Buffers are
      created through factory interfaces such as wl_shm, wp_linux_buffer_params
      (from the linux-dmabuf protocol extension) or similar. It has a width and
      a height and can be attached to a wl_surface, but the mechanism by which a
      client provides and updates the contents is defined by the buffer factory
      interface.

      If the buffer uses a format that has an alpha channel, the alpha channel
      is assumed to be premultiplied in the color channels unless otherwise
      specified.

      Note, because wl_buffer objects are created from multiple independent
      factory interfaces, the wl_buffer interface is frozen at version 1.
"))

(DEFMETHOD EVT-RELEASE ((OBJ WL_BUFFER) STREAM)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-RELEASE)))
    ";; compositor releases buffer

Sent when this wl_buffer is no longer used by the compositor.
	The client is now free to reuse or destroy this buffer and its
	backing storage.

	If a client receives a release event before the frame callback
	requested in the same wl_surface.commit that attaches this
	wl_buffer to a surface, then the client is immediately free to
	reuse the buffer and its backing storage, and does not need a
	second buffer for the next surface content update. Typically
	this is possible, when the compositor maintains a copy of the
	wl_surface contents, e.g. as a GL texture. This is an important
	optimization for GL(ES) compositors with wl_shm clients.
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD REQ-DESTROY ((OBJ WL_BUFFER) CLIENT)
  ";; destroy a buffer

Destroy a buffer. If and how you need to release the backing
	storage is defined by the buffer factory interface.

	For possible side-effects to a surface, see wl_surface.attach.
"
  (ERROR "Unimplemented"))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_BUFFER) EVENT)
  (CASE EVENT (EVT-RELEASE 0)))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_BUFFER) OPCODE)
  (NTH OPCODE '(REQ-DESTROY)))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_BUFFER) OPCODE) (NTH OPCODE '(NIL)))

(DEFPACKAGE :WL/WL_DATA_OFFER
  (:USE :CL :WL)
  (:EXPORT WL_DATA_OFFER
           REQ-ACCEPT
           REQ-RECEIVE
           REQ-DESTROY
           REQ-FINISH
           REQ-SET_ACTIONS
           EVT-OFFER
           EVT-SOURCE_ACTIONS
           EVT-ACTION))

(IN-PACKAGE :WL/WL_DATA_OFFER)

(DEFCLASS WL_DATA_OFFER (WL-OBJECT)
          ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "offer to transfer data

A wl_data_offer represents a piece of data offered for transfer
      by another client (the source client).  It is used by the
      copy-and-paste and drag-and-drop mechanisms.  The offer
      describes the different mime types that the data can be
      converted to and provides the mechanism for transferring the
      data directly from the source client.
"))

(DEFMETHOD EVT-OFFER ((OBJ WL_DATA_OFFER) STREAM MIME_TYPE)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-OFFER)))
    ";; advertise offered mime type

Sent immediately after creating the wl_data_offer object.  One
	event per offered mime type.

Arguments:
mime_type::string: offered mime type
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-SOURCE_ACTIONS ((OBJ WL_DATA_OFFER) STREAM SOURCE_ACTIONS)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-SOURCE_ACTIONS)))
    ";; notify the source-side available actions

This event indicates the actions offered by the data source. It
	will be sent immediately after creating the wl_data_offer object,
	or anytime the source side changes its offered actions through
	wl_data_source.set_actions.

Arguments:
source_actions::enum: actions offered by the data source
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-ACTION ((OBJ WL_DATA_OFFER) STREAM DND_ACTION)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-ACTION)))
    ";; notify the selected action

This event indicates the action selected by the compositor after
	matching the source/destination side actions. Only one action (or
	none) will be offered here.

	This event can be emitted multiple times during the drag-and-drop
	operation in response to destination side action changes through
	wl_data_offer.set_actions.

	This event will no longer be emitted after wl_data_device.drop
	happened on the drag-and-drop destination, the client must
	honor the last action received, or the last preferred one set
	through wl_data_offer.set_actions when handling an \"ask\" action.

	Compositors may also change the selected action on the fly, mainly
	in response to keyboard modifier changes during the drag-and-drop
	operation.

	The most recent action received is always the valid one. Prior to
	receiving wl_data_device.drop, the chosen action may change (e.g.
	due to keyboard modifiers being pressed). At the time of receiving
	wl_data_device.drop the drag-and-drop destination must honor the
	last action received.

	Action changes may still happen after wl_data_device.drop,
	especially on \"ask\" actions, where the drag-and-drop destination
	may choose another action afterwards. Action changes happening
	at this stage are always the result of inter-client negotiation, the
	compositor shall no longer be able to induce a different action.

	Upon \"ask\" actions, it is expected that the drag-and-drop destination
	may potentially choose a different action and/or mime type,
	based on wl_data_offer.source_actions and finally chosen by the
	user (e.g. popping up a menu with the available options). The
	final wl_data_offer.set_actions and wl_data_offer.accept requests
	must happen before the call to wl_data_offer.finish.

Arguments:
dnd_action::enum: action selected by the compositor
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD REQ-ACCEPT ((OBJ WL_DATA_OFFER) CLIENT SERIAL MIME_TYPE)
  ";; accept one of the offered mime types

Indicate that the client can accept the given mime type, or
	NULL for not accepted.

	For objects of version 2 or older, this request is used by the
	client to give feedback whether the client can receive the given
	mime type, or NULL if none is accepted; the feedback does not
	determine whether the drag-and-drop operation succeeds or not.

	For objects of version 3 or newer, this request determines the
	final result of the drag-and-drop operation. If the end result
	is that no mime types were accepted, the drag-and-drop operation
	will be cancelled and the corresponding drag source will receive
	wl_data_source.cancelled. Clients may still use this event in
	conjunction with wl_data_source.action for feedback.

Arguments:
serial::uint: serial number of the accept request
mime_type::string: mime type accepted by the client
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-RECEIVE ((OBJ WL_DATA_OFFER) CLIENT MIME_TYPE FD)
  ";; request that the data is transferred

To transfer the offered data, the client issues this request
	and indicates the mime type it wants to receive.  The transfer
	happens through the passed file descriptor (typically created
	with the pipe system call).  The source client writes the data
	in the mime type representation requested and then closes the
	file descriptor.

	The receiving client reads from the read end of the pipe until
	EOF and then closes its end, at which point the transfer is
	complete.

	This request may happen multiple times for different mime types,
	both before and after wl_data_device.drop. Drag-and-drop destination
	clients may preemptively fetch data or examine it more closely to
	determine acceptance.

Arguments:
mime_type::string: mime type desired by receiver
fd::fd: file descriptor for data transfer
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-DESTROY ((OBJ WL_DATA_OFFER) CLIENT)
  ";; destroy data offer

Destroy the data offer.
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-FINISH ((OBJ WL_DATA_OFFER) CLIENT)
  ";; the offer will no longer be used

Notifies the compositor that the drag destination successfully
	finished the drag-and-drop operation.

	Upon receiving this request, the compositor will emit
	wl_data_source.dnd_finished on the drag source client.

	It is a client error to perform other requests than
	wl_data_offer.destroy after this one. It is also an error to perform
	this request after a NULL mime type has been set in
	wl_data_offer.accept or no action was received through
	wl_data_offer.action.

	If wl_data_offer.finish request is received for a non drag and drop
	operation, the invalid_finish protocol error is raised.
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-SET_ACTIONS
           ((OBJ WL_DATA_OFFER) CLIENT DND_ACTIONS PREFERRED_ACTION)
  ";; set the available/preferred drag-and-drop actions

Sets the actions that the destination side client supports for
	this operation. This request may trigger the emission of
	wl_data_source.action and wl_data_offer.action events if the compositor
	needs to change the selected action.

	This request can be called multiple times throughout the
	drag-and-drop operation, typically in response to wl_data_device.enter
	or wl_data_device.motion events.

	This request determines the final result of the drag-and-drop
	operation. If the end result is that no action is accepted,
	the drag source will receive wl_data_source.cancelled.

	The dnd_actions argument must contain only values expressed in the
	wl_data_device_manager.dnd_actions enum, and the preferred_action
	argument must only contain one of those values set, otherwise it
	will result in a protocol error.

	While managing an \"ask\" action, the destination drag-and-drop client
	may perform further wl_data_offer.receive requests, and is expected
	to perform one last wl_data_offer.set_actions request with a preferred
	action other than \"ask\" (and optionally wl_data_offer.accept) before
	requesting wl_data_offer.finish, in order to convey the action selected
	by the user. If the preferred action is not in the
	wl_data_offer.source_actions mask, an error will be raised.

	If the \"ask\" action is dismissed (e.g. user cancellation), the client
	is expected to perform wl_data_offer.destroy right away.

	This request can only be made on drag-and-drop offers, a protocol error
	will be raised otherwise.

Arguments:
dnd_actions::enum: actions supported by the destination client
preferred_action::enum: action preferred by the destination client
"
  (ERROR "Unimplemented"))

(DEFMETHOD ENUM-ERROR ((OBJ WL_DATA_OFFER) VALUE)
  ";; "
  (CASE VALUE
    (0 'INVALID_FINISH)
    (1 'INVALID_ACTION_MASK)
    (2 'INVALID_ACTION)
    (3 'INVALID_OFFER)))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_DATA_OFFER) EVENT)
  (CASE EVENT (EVT-OFFER 0) (EVT-SOURCE_ACTIONS 1) (EVT-ACTION 2)))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_DATA_OFFER) OPCODE)
  (NTH OPCODE '(REQ-ACCEPT REQ-RECEIVE REQ-DESTROY REQ-FINISH REQ-SET_ACTIONS)))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_DATA_OFFER) OPCODE)
  (NTH OPCODE
       '((UINT STRING) (STRING FD) NIL NIL
         ((ENUM (WL_DATA_DEVICE_MANAGER DND_ACTION))
          (ENUM (WL_DATA_DEVICE_MANAGER DND_ACTION))))))

(DEFPACKAGE :WL/WL_DATA_SOURCE
  (:USE :CL :WL)
  (:EXPORT WL_DATA_SOURCE
           REQ-OFFER
           REQ-DESTROY
           REQ-SET_ACTIONS
           EVT-TARGET
           EVT-SEND
           EVT-CANCELLED
           EVT-DND_DROP_PERFORMED
           EVT-DND_FINISHED
           EVT-ACTION))

(IN-PACKAGE :WL/WL_DATA_SOURCE)

(DEFCLASS WL_DATA_SOURCE (WL-OBJECT)
          ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "offer to transfer data

The wl_data_source object is the source side of a wl_data_offer.
      It is created by the source client in a data transfer and
      provides a way to describe the offered data and a way to respond
      to requests to transfer the data.
"))

(DEFMETHOD EVT-TARGET ((OBJ WL_DATA_SOURCE) STREAM MIME_TYPE)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-TARGET)))
    ";; a target accepts an offered mime type

Sent when a target accepts pointer_focus or motion events.  If
	a target does not accept any of the offered types, type is NULL.

	Used for feedback during drag-and-drop.

Arguments:
mime_type::string: mime type accepted by the target
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-SEND ((OBJ WL_DATA_SOURCE) STREAM MIME_TYPE FD)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-SEND)))
    ";; send the data

Request for data from the client.  Send the data as the
	specified mime type over the passed file descriptor, then
	close it.

Arguments:
mime_type::string: mime type for the data
fd::fd: file descriptor for the data
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-CANCELLED ((OBJ WL_DATA_SOURCE) STREAM)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-CANCELLED)))
    ";; selection was cancelled

This data source is no longer valid. There are several reasons why
	this could happen:

	- The data source has been replaced by another data source.
	- The drag-and-drop operation was performed, but the drop destination
	  did not accept any of the mime types offered through
	  wl_data_source.target.
	- The drag-and-drop operation was performed, but the drop destination
	  did not select any of the actions present in the mask offered through
	  wl_data_source.action.
	- The drag-and-drop operation was performed but didn't happen over a
	  surface.
	- The compositor cancelled the drag-and-drop operation (e.g. compositor
	  dependent timeouts to avoid stale drag-and-drop transfers).

	The client should clean up and destroy this data source.

	For objects of version 2 or older, wl_data_source.cancelled will
	only be emitted if the data source was replaced by another data
	source.
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-DND_DROP_PERFORMED ((OBJ WL_DATA_SOURCE) STREAM)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-DND_DROP_PERFORMED)))
    ";; the drag-and-drop operation physically finished

The user performed the drop action. This event does not indicate
	acceptance, wl_data_source.cancelled may still be emitted afterwards
	if the drop destination does not accept any mime type.

	However, this event might however not be received if the compositor
	cancelled the drag-and-drop operation before this event could happen.

	Note that the data_source may still be used in the future and should
	not be destroyed here.
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-DND_FINISHED ((OBJ WL_DATA_SOURCE) STREAM)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-DND_FINISHED)))
    ";; the drag-and-drop operation concluded

The drop destination finished interoperating with this data
	source, so the client is now free to destroy this data source and
	free all associated data.

	If the action used to perform the operation was \"move\", the
	source can now delete the transferred data.
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-ACTION ((OBJ WL_DATA_SOURCE) STREAM DND_ACTION)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-ACTION)))
    ";; notify the selected action

This event indicates the action selected by the compositor after
	matching the source/destination side actions. Only one action (or
	none) will be offered here.

	This event can be emitted multiple times during the drag-and-drop
	operation, mainly in response to destination side changes through
	wl_data_offer.set_actions, and as the data device enters/leaves
	surfaces.

	It is only possible to receive this event after
	wl_data_source.dnd_drop_performed if the drag-and-drop operation
	ended in an \"ask\" action, in which case the final wl_data_source.action
	event will happen immediately before wl_data_source.dnd_finished.

	Compositors may also change the selected action on the fly, mainly
	in response to keyboard modifier changes during the drag-and-drop
	operation.

	The most recent action received is always the valid one. The chosen
	action may change alongside negotiation (e.g. an \"ask\" action can turn
	into a \"move\" operation), so the effects of the final action must
	always be applied in wl_data_offer.dnd_finished.

	Clients can trigger cursor surface changes from this point, so
	they reflect the current action.

Arguments:
dnd_action::enum: action selected by the compositor
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD REQ-OFFER ((OBJ WL_DATA_SOURCE) CLIENT MIME_TYPE)
  ";; add an offered mime type

This request adds a mime type to the set of mime types
	advertised to targets.  Can be called several times to offer
	multiple types.

Arguments:
mime_type::string: mime type offered by the data source
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-DESTROY ((OBJ WL_DATA_SOURCE) CLIENT)
  ";; destroy the data source

Destroy the data source.
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-SET_ACTIONS ((OBJ WL_DATA_SOURCE) CLIENT DND_ACTIONS)
  ";; set the available drag-and-drop actions

Sets the actions that the source side client supports for this
	operation. This request may trigger wl_data_source.action and
	wl_data_offer.action events if the compositor needs to change the
	selected action.

	The dnd_actions argument must contain only values expressed in the
	wl_data_device_manager.dnd_actions enum, otherwise it will result
	in a protocol error.

	This request must be made once only, and can only be made on sources
	used in drag-and-drop, so it must be performed before
	wl_data_device.start_drag. Attempting to use the source other than
	for drag-and-drop will raise a protocol error.

Arguments:
dnd_actions::enum: actions supported by the data source
"
  (ERROR "Unimplemented"))

(DEFMETHOD ENUM-ERROR ((OBJ WL_DATA_SOURCE) VALUE)
  ";; "
  (CASE VALUE (0 'INVALID_ACTION_MASK) (1 'INVALID_SOURCE)))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_DATA_SOURCE) EVENT)
  (CASE EVENT
    (EVT-TARGET 0)
    (EVT-SEND 1)
    (EVT-CANCELLED 2)
    (EVT-DND_DROP_PERFORMED 3)
    (EVT-DND_FINISHED 4)
    (EVT-ACTION 5)))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_DATA_SOURCE) OPCODE)
  (NTH OPCODE '(REQ-OFFER REQ-DESTROY REQ-SET_ACTIONS)))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_DATA_SOURCE) OPCODE)
  (NTH OPCODE '((STRING) NIL ((ENUM (WL_DATA_DEVICE_MANAGER DND_ACTION))))))

(DEFPACKAGE :WL/WL_DATA_DEVICE
  (:USE :CL :WL)
  (:EXPORT WL_DATA_DEVICE
           REQ-START_DRAG
           REQ-SET_SELECTION
           REQ-RELEASE
           EVT-DATA_OFFER
           EVT-ENTER
           EVT-LEAVE
           EVT-MOTION
           EVT-DROP
           EVT-SELECTION))

(IN-PACKAGE :WL/WL_DATA_DEVICE)

(DEFCLASS WL_DATA_DEVICE (WL-OBJECT)
          ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "data transfer device

There is one wl_data_device per seat which can be obtained
      from the global wl_data_device_manager singleton.

      A wl_data_device provides access to inter-client data transfer
      mechanisms such as copy-and-paste and drag-and-drop.
"))

(DEFMETHOD EVT-DATA_OFFER ((OBJ WL_DATA_DEVICE) STREAM ID)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-DATA_OFFER)))
    ";; introduce a new wl_data_offer

The data_offer event introduces a new wl_data_offer object,
	which will subsequently be used in either the
	data_device.enter event (for drag-and-drop) or the
	data_device.selection event (for selections).  Immediately
	following the data_device.data_offer event, the new data_offer
	object will send out data_offer.offer events to describe the
	mime types it offers.

Arguments:
id::new_id: the new data_offer object
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-ENTER ((OBJ WL_DATA_DEVICE) STREAM SERIAL SURFACE X Y ID)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-ENTER)))
    ";; initiate drag-and-drop session

This event is sent when an active drag-and-drop pointer enters
	a surface owned by the client.  The position of the pointer at
	enter time is provided by the x and y arguments, in surface-local
	coordinates.

Arguments:
serial::uint: serial number of the enter event
surface::object: client surface entered
x::fixed: surface-local x coordinate
y::fixed: surface-local y coordinate
id::object: source data_offer object
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-LEAVE ((OBJ WL_DATA_DEVICE) STREAM)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-LEAVE)))
    ";; end drag-and-drop session

This event is sent when the drag-and-drop pointer leaves the
	surface and the session ends.  The client must destroy the
	wl_data_offer introduced at enter time at this point.
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-MOTION ((OBJ WL_DATA_DEVICE) STREAM TIME X Y)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-MOTION)))
    ";; drag-and-drop session motion

This event is sent when the drag-and-drop pointer moves within
	the currently focused surface. The new position of the pointer
	is provided by the x and y arguments, in surface-local
	coordinates.

Arguments:
time::uint: timestamp with millisecond granularity
x::fixed: surface-local x coordinate
y::fixed: surface-local y coordinate
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-DROP ((OBJ WL_DATA_DEVICE) STREAM)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-DROP)))
    ";; end drag-and-drop session successfully

The event is sent when a drag-and-drop operation is ended
	because the implicit grab is removed.

	The drag-and-drop destination is expected to honor the last action
	received through wl_data_offer.action, if the resulting action is
	\"copy\" or \"move\", the destination can still perform
	wl_data_offer.receive requests, and is expected to end all
	transfers with a wl_data_offer.finish request.

	If the resulting action is \"ask\", the action will not be considered
	final. The drag-and-drop destination is expected to perform one last
	wl_data_offer.set_actions request, or wl_data_offer.destroy in order
	to cancel the operation.
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-SELECTION ((OBJ WL_DATA_DEVICE) STREAM ID)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-SELECTION)))
    ";; advertise new selection

The selection event is sent out to notify the client of a new
	wl_data_offer for the selection for this device.  The
	data_device.data_offer and the data_offer.offer events are
	sent out immediately before this event to introduce the data
	offer object.  The selection event is sent to a client
	immediately before receiving keyboard focus and when a new
	selection is set while the client has keyboard focus.  The
	data_offer is valid until a new data_offer or NULL is received
	or until the client loses keyboard focus.  Switching surface with
	keyboard focus within the same client doesn't mean a new selection
	will be sent.  The client must destroy the previous selection
	data_offer, if any, upon receiving this event.

Arguments:
id::object: selection data_offer object
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD REQ-START_DRAG
           ((OBJ WL_DATA_DEVICE) CLIENT SOURCE ORIGIN ICON SERIAL)
  ";; start drag-and-drop operation

This request asks the compositor to start a drag-and-drop
	operation on behalf of the client.

	The source argument is the data source that provides the data
	for the eventual data transfer. If source is NULL, enter, leave
	and motion events are sent only to the client that initiated the
	drag and the client is expected to handle the data passing
	internally. If source is destroyed, the drag-and-drop session will be
	cancelled.

	The origin surface is the surface where the drag originates and
	the client must have an active implicit grab that matches the
	serial.

	The icon surface is an optional (can be NULL) surface that
	provides an icon to be moved around with the cursor.  Initially,
	the top-left corner of the icon surface is placed at the cursor
	hotspot, but subsequent wl_surface.attach request can move the
	relative position. Attach requests must be confirmed with
	wl_surface.commit as usual. The icon surface is given the role of
	a drag-and-drop icon. If the icon surface already has another role,
	it raises a protocol error.

	The input region is ignored for wl_surfaces with the role of a
	drag-and-drop icon.

Arguments:
source::object: data source for the eventual transfer
origin::object: surface where the drag originates
icon::object: drag-and-drop icon surface
serial::uint: serial number of the implicit grab on the origin
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-SET_SELECTION ((OBJ WL_DATA_DEVICE) CLIENT SOURCE SERIAL)
  ";; copy data to the selection

This request asks the compositor to set the selection
	to the data from the source on behalf of the client.

	To unset the selection, set the source to NULL.

Arguments:
source::object: data source for the selection
serial::uint: serial number of the event that triggered this request
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-RELEASE ((OBJ WL_DATA_DEVICE) CLIENT)
  ";; destroy data device

This request destroys the data device.
"
  (ERROR "Unimplemented"))

(DEFMETHOD ENUM-ERROR ((OBJ WL_DATA_DEVICE) VALUE) ";; " (CASE VALUE (0 'ROLE)))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_DATA_DEVICE) EVENT)
  (CASE EVENT
    (EVT-DATA_OFFER 0)
    (EVT-ENTER 1)
    (EVT-LEAVE 2)
    (EVT-MOTION 3)
    (EVT-DROP 4)
    (EVT-SELECTION 5)))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_DATA_DEVICE) OPCODE)
  (NTH OPCODE '(REQ-START_DRAG REQ-SET_SELECTION REQ-RELEASE)))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_DATA_DEVICE) OPCODE)
  (NTH OPCODE '((OBJECT OBJECT OBJECT UINT) (OBJECT UINT) NIL)))

(DEFPACKAGE :WL/WL_DATA_DEVICE_MANAGER
  (:USE :CL :WL)
  (:EXPORT WL_DATA_DEVICE_MANAGER REQ-CREATE_DATA_SOURCE REQ-GET_DATA_DEVICE))

(IN-PACKAGE :WL/WL_DATA_DEVICE_MANAGER)

(DEFCLASS WL_DATA_DEVICE_MANAGER (WL-OBJECT)
          ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "data transfer interface

The wl_data_device_manager is a singleton global object that
      provides access to inter-client data transfer mechanisms such as
      copy-and-paste and drag-and-drop.  These mechanisms are tied to
      a wl_seat and this interface lets a client get a wl_data_device
      corresponding to a wl_seat.

      Depending on the version bound, the objects created from the bound
      wl_data_device_manager object will have different requirements for
      functioning properly. See wl_data_source.set_actions,
      wl_data_offer.accept and wl_data_offer.finish for details.
"))

(DEFMETHOD REQ-CREATE_DATA_SOURCE ((OBJ WL_DATA_DEVICE_MANAGER) CLIENT ID)
  ";; create a new data source

Create a new data source.

Arguments:
id::new_id: data source to create
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-GET_DATA_DEVICE ((OBJ WL_DATA_DEVICE_MANAGER) CLIENT ID SEAT)
  ";; create a new data device

Create a new data device for a given seat.

Arguments:
id::new_id: data device to create
seat::object: seat associated with the data device
"
  (ERROR "Unimplemented"))

(DEFMETHOD ENUM-DND_ACTION ((OBJ WL_DATA_DEVICE_MANAGER) VALUE)
  ";; drag and drop actions

This is a bitmask of the available/preferred actions in a
	drag-and-drop operation.

	In the compositor, the selected action is a result of matching the
	actions offered by the source and destination sides.  \"action\" events
	with a \"none\" action will be sent to both source and destination if
	there is no match. All further checks will effectively happen on
	(source actions ∩ destination actions).

	In addition, compositors may also pick different actions in
	reaction to key modifiers being pressed. One common design that
	is used in major toolkits (and the behavior recommended for
	compositors) is:

	- If no modifiers are pressed, the first match (in bit order)
	  will be used.
	- Pressing Shift selects \"move\", if enabled in the mask.
	- Pressing Control selects \"copy\", if enabled in the mask.

	Behavior beyond that is considered implementation-dependent.
	Compositors may for example bind other modifiers (like Alt/Meta)
	or drags initiated with other buttons than BTN_LEFT to specific
	actions (e.g. \"ask\").
"
  (LET ((OPTIONS
         '((0 READ-FROM-STRING (NAME ENTRY)) (1 READ-FROM-STRING (NAME ENTRY))
           (2 READ-FROM-STRING (NAME ENTRY))
           (4 READ-FROM-STRING (NAME ENTRY)))))
    (LOOP FOR (MASK NAME) IN OPTIONS
          WHEN (LOGBITP MASK VALUE)
          COLLECT NAME)))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_DATA_DEVICE_MANAGER) EVENT) (CASE EVENT))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_DATA_DEVICE_MANAGER) OPCODE)
  (NTH OPCODE '(REQ-CREATE_DATA_SOURCE REQ-GET_DATA_DEVICE)))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_DATA_DEVICE_MANAGER) OPCODE)
  (NTH OPCODE '((NEW_ID) (NEW_ID OBJECT))))

(DEFPACKAGE :WL/WL_SHELL
  (:USE :CL :WL)
  (:EXPORT WL_SHELL REQ-GET_SHELL_SURFACE))

(IN-PACKAGE :WL/WL_SHELL)

(DEFCLASS WL_SHELL (WL-OBJECT) ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "create desktop-style surfaces

This interface is implemented by servers that provide
      desktop-style user interfaces.

      It allows clients to associate a wl_shell_surface with
      a basic surface.

      Note! This protocol is deprecated and not intended for production use.
      For desktop-style user interfaces, use xdg_shell. Compositors and clients
      should not implement this interface.
"))

(DEFMETHOD REQ-GET_SHELL_SURFACE ((OBJ WL_SHELL) CLIENT ID SURFACE)
  ";; create a shell surface from a surface

Create a shell surface for an existing surface. This gives
	the wl_surface the role of a shell surface. If the wl_surface
	already has another role, it raises a protocol error.

	Only one shell surface can be associated with a given surface.

Arguments:
id::new_id: shell surface to create
surface::object: surface to be given the shell surface role
"
  (ERROR "Unimplemented"))

(DEFMETHOD ENUM-ERROR ((OBJ WL_SHELL) VALUE) ";; " (CASE VALUE (0 'ROLE)))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_SHELL) EVENT) (CASE EVENT))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_SHELL) OPCODE)
  (NTH OPCODE '(REQ-GET_SHELL_SURFACE)))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_SHELL) OPCODE)
  (NTH OPCODE '((NEW_ID OBJECT))))

(DEFPACKAGE :WL/WL_SHELL_SURFACE
  (:USE :CL :WL)
  (:EXPORT WL_SHELL_SURFACE
           REQ-PONG
           REQ-MOVE
           REQ-RESIZE
           REQ-SET_TOPLEVEL
           REQ-SET_TRANSIENT
           REQ-SET_FULLSCREEN
           REQ-SET_POPUP
           REQ-SET_MAXIMIZED
           REQ-SET_TITLE
           REQ-SET_CLASS
           EVT-PING
           EVT-CONFIGURE
           EVT-POPUP_DONE))

(IN-PACKAGE :WL/WL_SHELL_SURFACE)

(DEFCLASS WL_SHELL_SURFACE (WL-OBJECT)
          ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "desktop-style metadata interface

An interface that may be implemented by a wl_surface, for
      implementations that provide a desktop-style user interface.

      It provides requests to treat surfaces like toplevel, fullscreen
      or popup windows, move, resize or maximize them, associate
      metadata like title and class, etc.

      On the server side the object is automatically destroyed when
      the related wl_surface is destroyed. On the client side,
      wl_shell_surface_destroy() must be called before destroying
      the wl_surface object.
"))

(DEFMETHOD EVT-PING ((OBJ WL_SHELL_SURFACE) STREAM SERIAL)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-PING)))
    ";; ping client

Ping a client to check if it is receiving events and sending
	requests. A client is expected to reply with a pong request.

Arguments:
serial::uint: serial number of the ping
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-CONFIGURE ((OBJ WL_SHELL_SURFACE) STREAM EDGES WIDTH HEIGHT)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-CONFIGURE)))
    ";; suggest resize

The configure event asks the client to resize its surface.

	The size is a hint, in the sense that the client is free to
	ignore it if it doesn't resize, pick a smaller size (to
	satisfy aspect ratio or resize in steps of NxM pixels).

	The edges parameter provides a hint about how the surface
	was resized. The client may use this information to decide
	how to adjust its content to the new size (e.g. a scrolling
	area might adjust its content position to leave the viewable
	content unmoved).

	The client is free to dismiss all but the last configure
	event it received.

	The width and height arguments specify the size of the window
	in surface-local coordinates.

Arguments:
edges::enum: how the surface was resized
width::int: new width of the surface
height::int: new height of the surface
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-POPUP_DONE ((OBJ WL_SHELL_SURFACE) STREAM)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-POPUP_DONE)))
    ";; popup interaction is done

The popup_done event is sent out when a popup grab is broken,
	that is, when the user clicks a surface that doesn't belong
	to the client owning the popup surface.
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD REQ-PONG ((OBJ WL_SHELL_SURFACE) CLIENT SERIAL)
  ";; respond to a ping event

A client must respond to a ping event with a pong request or
	the client may be deemed unresponsive.

Arguments:
serial::uint: serial number of the ping event
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-MOVE ((OBJ WL_SHELL_SURFACE) CLIENT SEAT SERIAL)
  ";; start an interactive move

Start a pointer-driven move of the surface.

	This request must be used in response to a button press event.
	The server may ignore move requests depending on the state of
	the surface (e.g. fullscreen or maximized).

Arguments:
seat::object: seat whose pointer is used
serial::uint: serial number of the implicit grab on the pointer
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-RESIZE ((OBJ WL_SHELL_SURFACE) CLIENT SEAT SERIAL EDGES)
  ";; start an interactive resize

Start a pointer-driven resizing of the surface.

	This request must be used in response to a button press event.
	The server may ignore resize requests depending on the state of
	the surface (e.g. fullscreen or maximized).

Arguments:
seat::object: seat whose pointer is used
serial::uint: serial number of the implicit grab on the pointer
edges::enum: which edge or corner is being dragged
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-SET_TOPLEVEL ((OBJ WL_SHELL_SURFACE) CLIENT)
  ";; make the surface a toplevel surface

Map the surface as a toplevel surface.

	A toplevel surface is not fullscreen, maximized or transient.
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-SET_TRANSIENT ((OBJ WL_SHELL_SURFACE) CLIENT PARENT X Y FLAGS)
  ";; make the surface a transient surface

Map the surface relative to an existing surface.

	The x and y arguments specify the location of the upper left
	corner of the surface relative to the upper left corner of the
	parent surface, in surface-local coordinates.

	The flags argument controls details of the transient behaviour.

Arguments:
parent::object: parent surface
x::int: surface-local x coordinate
y::int: surface-local y coordinate
flags::enum: transient surface behavior
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-SET_FULLSCREEN
           ((OBJ WL_SHELL_SURFACE) CLIENT METHOD FRAMERATE OUTPUT)
  ";; make the surface a fullscreen surface

Map the surface as a fullscreen surface.

	If an output parameter is given then the surface will be made
	fullscreen on that output. If the client does not specify the
	output then the compositor will apply its policy - usually
	choosing the output on which the surface has the biggest surface
	area.

	The client may specify a method to resolve a size conflict
	between the output size and the surface size - this is provided
	through the method parameter.

	The framerate parameter is used only when the method is set
	to \"driver\", to indicate the preferred framerate. A value of 0
	indicates that the client does not care about framerate.  The
	framerate is specified in mHz, that is framerate of 60000 is 60Hz.

	A method of \"scale\" or \"driver\" implies a scaling operation of
	the surface, either via a direct scaling operation or a change of
	the output mode. This will override any kind of output scaling, so
	that mapping a surface with a buffer size equal to the mode can
	fill the screen independent of buffer_scale.

	A method of \"fill\" means we don't scale up the buffer, however
	any output scale is applied. This means that you may run into
	an edge case where the application maps a buffer with the same
	size of the output mode but buffer_scale 1 (thus making a
	surface larger than the output). In this case it is allowed to
	downscale the results to fit the screen.

	The compositor must reply to this request with a configure event
	with the dimensions for the output on which the surface will
	be made fullscreen.

Arguments:
method::enum: method for resolving size conflict
framerate::uint: framerate in mHz
output::object: output on which the surface is to be fullscreen
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-SET_POPUP
           ((OBJ WL_SHELL_SURFACE) CLIENT SEAT SERIAL PARENT X Y FLAGS)
  ";; make the surface a popup surface

Map the surface as a popup.

	A popup surface is a transient surface with an added pointer
	grab.

	An existing implicit grab will be changed to owner-events mode,
	and the popup grab will continue after the implicit grab ends
	(i.e. releasing the mouse button does not cause the popup to
	be unmapped).

	The popup grab continues until the window is destroyed or a
	mouse button is pressed in any other client's window. A click
	in any of the client's surfaces is reported as normal, however,
	clicks in other clients' surfaces will be discarded and trigger
	the callback.

	The x and y arguments specify the location of the upper left
	corner of the surface relative to the upper left corner of the
	parent surface, in surface-local coordinates.

Arguments:
seat::object: seat whose pointer is used
serial::uint: serial number of the implicit grab on the pointer
parent::object: parent surface
x::int: surface-local x coordinate
y::int: surface-local y coordinate
flags::enum: transient surface behavior
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-SET_MAXIMIZED ((OBJ WL_SHELL_SURFACE) CLIENT OUTPUT)
  ";; make the surface a maximized surface

Map the surface as a maximized surface.

	If an output parameter is given then the surface will be
	maximized on that output. If the client does not specify the
	output then the compositor will apply its policy - usually
	choosing the output on which the surface has the biggest surface
	area.

	The compositor will reply with a configure event telling
	the expected new surface size. The operation is completed
	on the next buffer attach to this surface.

	A maximized surface typically fills the entire output it is
	bound to, except for desktop elements such as panels. This is
	the main difference between a maximized shell surface and a
	fullscreen shell surface.

	The details depend on the compositor implementation.

Arguments:
output::object: output on which the surface is to be maximized
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-SET_TITLE ((OBJ WL_SHELL_SURFACE) CLIENT TITLE)
  ";; set surface title

Set a short title for the surface.

	This string may be used to identify the surface in a task bar,
	window list, or other user interface elements provided by the
	compositor.

	The string must be encoded in UTF-8.

Arguments:
title::string: surface title
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-SET_CLASS ((OBJ WL_SHELL_SURFACE) CLIENT CLASS_)
  ";; set surface class

Set a class for the surface.

	The surface class identifies the general class of applications
	to which the surface belongs. A common convention is to use the
	file name (or the full path if it is a non-standard location) of
	the application's .desktop file as the class.

Arguments:
class_::string: surface class
"
  (ERROR "Unimplemented"))

(DEFMETHOD ENUM-RESIZE ((OBJ WL_SHELL_SURFACE) VALUE)
  ";; edge values for resizing

These values are used to indicate which edge of a surface
	is being dragged in a resize operation. The server may
	use this information to adapt its behavior, e.g. choose
	an appropriate cursor image.
"
  (LET ((OPTIONS
         '((0 READ-FROM-STRING (NAME ENTRY)) (1 READ-FROM-STRING (NAME ENTRY))
           (2 READ-FROM-STRING (NAME ENTRY)) (4 READ-FROM-STRING (NAME ENTRY))
           (5 READ-FROM-STRING (NAME ENTRY)) (6 READ-FROM-STRING (NAME ENTRY))
           (8 READ-FROM-STRING (NAME ENTRY)) (9 READ-FROM-STRING (NAME ENTRY))
           (10 READ-FROM-STRING (NAME ENTRY)))))
    (LOOP FOR (MASK NAME) IN OPTIONS
          WHEN (LOGBITP MASK VALUE)
          COLLECT NAME)))

(DEFMETHOD ENUM-TRANSIENT ((OBJ WL_SHELL_SURFACE) VALUE)
  ";; details of transient behaviour

These flags specify details of the expected behaviour
	of transient surfaces. Used in the set_transient request.
"
  (LET ((OPTIONS '((1 READ-FROM-STRING (NAME ENTRY)))))
    (LOOP FOR (MASK NAME) IN OPTIONS
          WHEN (LOGBITP MASK VALUE)
          COLLECT NAME)))

(DEFMETHOD ENUM-FULLSCREEN_METHOD ((OBJ WL_SHELL_SURFACE) VALUE)
  ";; different method to set the surface fullscreen

Hints to indicate to the compositor how to deal with a conflict
	between the dimensions of the surface and the dimensions of the
	output. The compositor is free to ignore this parameter.
"
  (CASE VALUE (0 'DEFAULT) (1 'SCALE) (2 'DRIVER) (3 'FILL)))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_SHELL_SURFACE) EVENT)
  (CASE EVENT (EVT-PING 0) (EVT-CONFIGURE 1) (EVT-POPUP_DONE 2)))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_SHELL_SURFACE) OPCODE)
  (NTH OPCODE
       '(REQ-PONG REQ-MOVE REQ-RESIZE REQ-SET_TOPLEVEL REQ-SET_TRANSIENT
         REQ-SET_FULLSCREEN REQ-SET_POPUP REQ-SET_MAXIMIZED REQ-SET_TITLE
         REQ-SET_CLASS)))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_SHELL_SURFACE) OPCODE)
  (NTH OPCODE
       '((UINT) (OBJECT UINT) (OBJECT UINT (ENUM (RESIZE))) NIL
         (OBJECT INT INT (ENUM (TRANSIENT)))
         ((ENUM (FULLSCREEN_METHOD)) UINT OBJECT)
         (OBJECT UINT OBJECT INT INT (ENUM (TRANSIENT))) (OBJECT) (STRING)
         (STRING))))

(DEFPACKAGE :WL/WL_SURFACE
  (:USE :CL :WL)
  (:EXPORT WL_SURFACE
           REQ-DESTROY
           REQ-ATTACH
           REQ-DAMAGE
           REQ-FRAME
           REQ-SET_OPAQUE_REGION
           REQ-SET_INPUT_REGION
           REQ-COMMIT
           REQ-SET_BUFFER_TRANSFORM
           REQ-SET_BUFFER_SCALE
           REQ-DAMAGE_BUFFER
           REQ-OFFSET
           EVT-ENTER
           EVT-LEAVE
           EVT-PREFERRED_BUFFER_SCALE
           EVT-PREFERRED_BUFFER_TRANSFORM))

(IN-PACKAGE :WL/WL_SURFACE)

(DEFCLASS WL_SURFACE (WL-OBJECT) ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "an onscreen surface

A surface is a rectangular area that may be displayed on zero
      or more outputs, and shown any number of times at the compositor's
      discretion. They can present wl_buffers, receive user input, and
      define a local coordinate system.

      The size of a surface (and relative positions on it) is described
      in surface-local coordinates, which may differ from the buffer
      coordinates of the pixel content, in case a buffer_transform
      or a buffer_scale is used.

      A surface without a \"role\" is fairly useless: a compositor does
      not know where, when or how to present it. The role is the
      purpose of a wl_surface. Examples of roles are a cursor for a
      pointer (as set by wl_pointer.set_cursor), a drag icon
      (wl_data_device.start_drag), a sub-surface
      (wl_subcompositor.get_subsurface), and a window as defined by a
      shell protocol (e.g. wl_shell.get_shell_surface).

      A surface can have only one role at a time. Initially a
      wl_surface does not have a role. Once a wl_surface is given a
      role, it is set permanently for the whole lifetime of the
      wl_surface object. Giving the current role again is allowed,
      unless explicitly forbidden by the relevant interface
      specification.

      Surface roles are given by requests in other interfaces such as
      wl_pointer.set_cursor. The request should explicitly mention
      that this request gives a role to a wl_surface. Often, this
      request also creates a new protocol object that represents the
      role and adds additional functionality to wl_surface. When a
      client wants to destroy a wl_surface, they must destroy this role
      object before the wl_surface, otherwise a defunct_role_object error is
      sent.

      Destroying the role object does not remove the role from the
      wl_surface, but it may stop the wl_surface from \"playing the role\".
      For instance, if a wl_subsurface object is destroyed, the wl_surface
      it was created for will be unmapped and forget its position and
      z-order. It is allowed to create a wl_subsurface for the same
      wl_surface again, but it is not allowed to use the wl_surface as
      a cursor (cursor is a different role than sub-surface, and role
      switching is not allowed).
"))

(DEFMETHOD EVT-ENTER ((OBJ WL_SURFACE) STREAM OUTPUT)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-ENTER)))
    ";; surface enters an output

This is emitted whenever a surface's creation, movement, or resizing
	results in some part of it being within the scanout region of an
	output.

	Note that a surface may be overlapping with zero or more outputs.

Arguments:
output::object: output entered by the surface
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-LEAVE ((OBJ WL_SURFACE) STREAM OUTPUT)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-LEAVE)))
    ";; surface leaves an output

This is emitted whenever a surface's creation, movement, or resizing
	results in it no longer having any part of it within the scanout region
	of an output.

	Clients should not use the number of outputs the surface is on for frame
	throttling purposes. The surface might be hidden even if no leave event
	has been sent, and the compositor might expect new surface content
	updates even if no enter event has been sent. The frame event should be
	used instead.

Arguments:
output::object: output left by the surface
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-PREFERRED_BUFFER_SCALE ((OBJ WL_SURFACE) STREAM FACTOR)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-PREFERRED_BUFFER_SCALE)))
    ";; preferred buffer scale for the surface

This event indicates the preferred buffer scale for this surface. It is
	sent whenever the compositor's preference changes.

	It is intended that scaling aware clients use this event to scale their
	content and use wl_surface.set_buffer_scale to indicate the scale they
	have rendered with. This allows clients to supply a higher detail
	buffer.

Arguments:
factor::int: preferred scaling factor
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-PREFERRED_BUFFER_TRANSFORM ((OBJ WL_SURFACE) STREAM TRANSFORM)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-PREFERRED_BUFFER_TRANSFORM)))
    ";; preferred buffer transform for the surface

This event indicates the preferred buffer transform for this surface.
	It is sent whenever the compositor's preference changes.

	It is intended that transform aware clients use this event to apply the
	transform to their content and use wl_surface.set_buffer_transform to
	indicate the transform they have rendered with.

Arguments:
transform::enum: preferred transform
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD REQ-DESTROY ((OBJ WL_SURFACE) CLIENT)
  ";; delete surface

Deletes the surface and invalidates its object ID.
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-ATTACH ((OBJ WL_SURFACE) CLIENT BUFFER X Y)
  ";; set the surface contents

Set a buffer as the content of this surface.

	The new size of the surface is calculated based on the buffer
	size transformed by the inverse buffer_transform and the
	inverse buffer_scale. This means that at commit time the supplied
	buffer size must be an integer multiple of the buffer_scale. If
	that's not the case, an invalid_size error is sent.

	The x and y arguments specify the location of the new pending
	buffer's upper left corner, relative to the current buffer's upper
	left corner, in surface-local coordinates. In other words, the
	x and y, combined with the new surface size define in which
	directions the surface's size changes. Setting anything other than 0
	as x and y arguments is discouraged, and should instead be replaced
	with using the separate wl_surface.offset request.

	When the bound wl_surface version is 5 or higher, passing any
	non-zero x or y is a protocol violation, and will result in an
        'invalid_offset' error being raised. The x and y arguments are ignored
        and do not change the pending state. To achieve equivalent semantics,
        use wl_surface.offset.

	Surface contents are double-buffered state, see wl_surface.commit.

	The initial surface contents are void; there is no content.
	wl_surface.attach assigns the given wl_buffer as the pending
	wl_buffer. wl_surface.commit makes the pending wl_buffer the new
	surface contents, and the size of the surface becomes the size
	calculated from the wl_buffer, as described above. After commit,
	there is no pending buffer until the next attach.

	Committing a pending wl_buffer allows the compositor to read the
	pixels in the wl_buffer. The compositor may access the pixels at
	any time after the wl_surface.commit request. When the compositor
	will not access the pixels anymore, it will send the
	wl_buffer.release event. Only after receiving wl_buffer.release,
	the client may reuse the wl_buffer. A wl_buffer that has been
	attached and then replaced by another attach instead of committed
	will not receive a release event, and is not used by the
	compositor.

	If a pending wl_buffer has been committed to more than one wl_surface,
	the delivery of wl_buffer.release events becomes undefined. A well
	behaved client should not rely on wl_buffer.release events in this
	case. Alternatively, a client could create multiple wl_buffer objects
	from the same backing storage or use wp_linux_buffer_release.

	Destroying the wl_buffer after wl_buffer.release does not change
	the surface contents. Destroying the wl_buffer before wl_buffer.release
	is allowed as long as the underlying buffer storage isn't re-used (this
	can happen e.g. on client process termination). However, if the client
	destroys the wl_buffer before receiving the wl_buffer.release event and
	mutates the underlying buffer storage, the surface contents become
	undefined immediately.

	If wl_surface.attach is sent with a NULL wl_buffer, the
	following wl_surface.commit will remove the surface content.

Arguments:
buffer::object: buffer of surface contents
x::int: surface-local x coordinate
y::int: surface-local y coordinate
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-DAMAGE ((OBJ WL_SURFACE) CLIENT X Y WIDTH HEIGHT)
  ";; mark part of the surface damaged

This request is used to describe the regions where the pending
	buffer is different from the current surface contents, and where
	the surface therefore needs to be repainted. The compositor
	ignores the parts of the damage that fall outside of the surface.

	Damage is double-buffered state, see wl_surface.commit.

	The damage rectangle is specified in surface-local coordinates,
	where x and y specify the upper left corner of the damage rectangle.

	The initial value for pending damage is empty: no damage.
	wl_surface.damage adds pending damage: the new pending damage
	is the union of old pending damage and the given rectangle.

	wl_surface.commit assigns pending damage as the current damage,
	and clears pending damage. The server will clear the current
	damage as it repaints the surface.

	Note! New clients should not use this request. Instead damage can be
	posted with wl_surface.damage_buffer which uses buffer coordinates
	instead of surface coordinates.

Arguments:
x::int: surface-local x coordinate
y::int: surface-local y coordinate
width::int: width of damage rectangle
height::int: height of damage rectangle
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-FRAME ((OBJ WL_SURFACE) CLIENT CALLBACK)
  ";; request a frame throttling hint

Request a notification when it is a good time to start drawing a new
	frame, by creating a frame callback. This is useful for throttling
	redrawing operations, and driving animations.

	When a client is animating on a wl_surface, it can use the 'frame'
	request to get notified when it is a good time to draw and commit the
	next frame of animation. If the client commits an update earlier than
	that, it is likely that some updates will not make it to the display,
	and the client is wasting resources by drawing too often.

	The frame request will take effect on the next wl_surface.commit.
	The notification will only be posted for one frame unless
	requested again. For a wl_surface, the notifications are posted in
	the order the frame requests were committed.

	The server must send the notifications so that a client
	will not send excessive updates, while still allowing
	the highest possible update rate for clients that wait for the reply
	before drawing again. The server should give some time for the client
	to draw and commit after sending the frame callback events to let it
	hit the next output refresh.

	A server should avoid signaling the frame callbacks if the
	surface is not visible in any way, e.g. the surface is off-screen,
	or completely obscured by other opaque surfaces.

	The object returned by this request will be destroyed by the
	compositor after the callback is fired and as such the client must not
	attempt to use it after that point.

	The callback_data passed in the callback is the current time, in
	milliseconds, with an undefined base.

Arguments:
callback::new_id: callback object for the frame request
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-SET_OPAQUE_REGION ((OBJ WL_SURFACE) CLIENT REGION)
  ";; set opaque region

This request sets the region of the surface that contains
	opaque content.

	The opaque region is an optimization hint for the compositor
	that lets it optimize the redrawing of content behind opaque
	regions.  Setting an opaque region is not required for correct
	behaviour, but marking transparent content as opaque will result
	in repaint artifacts.

	The opaque region is specified in surface-local coordinates.

	The compositor ignores the parts of the opaque region that fall
	outside of the surface.

	Opaque region is double-buffered state, see wl_surface.commit.

	wl_surface.set_opaque_region changes the pending opaque region.
	wl_surface.commit copies the pending region to the current region.
	Otherwise, the pending and current regions are never changed.

	The initial value for an opaque region is empty. Setting the pending
	opaque region has copy semantics, and the wl_region object can be
	destroyed immediately. A NULL wl_region causes the pending opaque
	region to be set to empty.

Arguments:
region::object: opaque region of the surface
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-SET_INPUT_REGION ((OBJ WL_SURFACE) CLIENT REGION)
  ";; set input region

This request sets the region of the surface that can receive
	pointer and touch events.

	Input events happening outside of this region will try the next
	surface in the server surface stack. The compositor ignores the
	parts of the input region that fall outside of the surface.

	The input region is specified in surface-local coordinates.

	Input region is double-buffered state, see wl_surface.commit.

	wl_surface.set_input_region changes the pending input region.
	wl_surface.commit copies the pending region to the current region.
	Otherwise the pending and current regions are never changed,
	except cursor and icon surfaces are special cases, see
	wl_pointer.set_cursor and wl_data_device.start_drag.

	The initial value for an input region is infinite. That means the
	whole surface will accept input. Setting the pending input region
	has copy semantics, and the wl_region object can be destroyed
	immediately. A NULL wl_region causes the input region to be set
	to infinite.

Arguments:
region::object: input region of the surface
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-COMMIT ((OBJ WL_SURFACE) CLIENT)
  ";; commit pending surface state

Surface state (input, opaque, and damage regions, attached buffers,
	etc.) is double-buffered. Protocol requests modify the pending state,
	as opposed to the current state in use by the compositor. A commit
	request atomically applies all pending state, replacing the current
	state. After commit, the new pending state is as documented for each
	related request.

	On commit, a pending wl_buffer is applied first, and all other state
	second. This means that all coordinates in double-buffered state are
	relative to the new wl_buffer coming into use, except for
	wl_surface.attach itself. If there is no pending wl_buffer, the
	coordinates are relative to the current surface contents.

	All requests that need a commit to become effective are documented
	to affect double-buffered state.

	Other interfaces may add further double-buffered surface state.
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-SET_BUFFER_TRANSFORM ((OBJ WL_SURFACE) CLIENT TRANSFORM)
  ";; sets the buffer transformation

This request sets an optional transformation on how the compositor
	interprets the contents of the buffer attached to the surface. The
	accepted values for the transform parameter are the values for
	wl_output.transform.

	Buffer transform is double-buffered state, see wl_surface.commit.

	A newly created surface has its buffer transformation set to normal.

	wl_surface.set_buffer_transform changes the pending buffer
	transformation. wl_surface.commit copies the pending buffer
	transformation to the current one. Otherwise, the pending and current
	values are never changed.

	The purpose of this request is to allow clients to render content
	according to the output transform, thus permitting the compositor to
	use certain optimizations even if the display is rotated. Using
	hardware overlays and scanning out a client buffer for fullscreen
	surfaces are examples of such optimizations. Those optimizations are
	highly dependent on the compositor implementation, so the use of this
	request should be considered on a case-by-case basis.

	Note that if the transform value includes 90 or 270 degree rotation,
	the width of the buffer will become the surface height and the height
	of the buffer will become the surface width.

	If transform is not one of the values from the
	wl_output.transform enum the invalid_transform protocol error
	is raised.

Arguments:
transform::enum: transform for interpreting buffer contents
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-SET_BUFFER_SCALE ((OBJ WL_SURFACE) CLIENT SCALE)
  ";; sets the buffer scaling factor

This request sets an optional scaling factor on how the compositor
	interprets the contents of the buffer attached to the window.

	Buffer scale is double-buffered state, see wl_surface.commit.

	A newly created surface has its buffer scale set to 1.

	wl_surface.set_buffer_scale changes the pending buffer scale.
	wl_surface.commit copies the pending buffer scale to the current one.
	Otherwise, the pending and current values are never changed.

	The purpose of this request is to allow clients to supply higher
	resolution buffer data for use on high resolution outputs. It is
	intended that you pick the same buffer scale as the scale of the
	output that the surface is displayed on. This means the compositor
	can avoid scaling when rendering the surface on that output.

	Note that if the scale is larger than 1, then you have to attach
	a buffer that is larger (by a factor of scale in each dimension)
	than the desired surface size.

	If scale is not positive the invalid_scale protocol error is
	raised.

Arguments:
scale::int: positive scale for interpreting buffer contents
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-DAMAGE_BUFFER ((OBJ WL_SURFACE) CLIENT X Y WIDTH HEIGHT)
  ";; mark part of the surface damaged using buffer coordinates

This request is used to describe the regions where the pending
	buffer is different from the current surface contents, and where
	the surface therefore needs to be repainted. The compositor
	ignores the parts of the damage that fall outside of the surface.

	Damage is double-buffered state, see wl_surface.commit.

	The damage rectangle is specified in buffer coordinates,
	where x and y specify the upper left corner of the damage rectangle.

	The initial value for pending damage is empty: no damage.
	wl_surface.damage_buffer adds pending damage: the new pending
	damage is the union of old pending damage and the given rectangle.

	wl_surface.commit assigns pending damage as the current damage,
	and clears pending damage. The server will clear the current
	damage as it repaints the surface.

	This request differs from wl_surface.damage in only one way - it
	takes damage in buffer coordinates instead of surface-local
	coordinates. While this generally is more intuitive than surface
	coordinates, it is especially desirable when using wp_viewport
	or when a drawing library (like EGL) is unaware of buffer scale
	and buffer transform.

	Note: Because buffer transformation changes and damage requests may
	be interleaved in the protocol stream, it is impossible to determine
	the actual mapping between surface and buffer damage until
	wl_surface.commit time. Therefore, compositors wishing to take both
	kinds of damage into account will have to accumulate damage from the
	two requests separately and only transform from one to the other
	after receiving the wl_surface.commit.

Arguments:
x::int: buffer-local x coordinate
y::int: buffer-local y coordinate
width::int: width of damage rectangle
height::int: height of damage rectangle
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-OFFSET ((OBJ WL_SURFACE) CLIENT X Y)
  ";; set the surface contents offset

The x and y arguments specify the location of the new pending
	buffer's upper left corner, relative to the current buffer's upper
	left corner, in surface-local coordinates. In other words, the
	x and y, combined with the new surface size define in which
	directions the surface's size changes.

	Surface location offset is double-buffered state, see
	wl_surface.commit.

	This request is semantically equivalent to and the replaces the x and y
	arguments in the wl_surface.attach request in wl_surface versions prior
	to 5. See wl_surface.attach for details.

Arguments:
x::int: surface-local x coordinate
y::int: surface-local y coordinate
"
  (ERROR "Unimplemented"))

(DEFMETHOD ENUM-ERROR ((OBJ WL_SURFACE) VALUE)
  ";; wl_surface error values

These errors can be emitted in response to wl_surface requests.
"
  (CASE VALUE
    (0 'INVALID_SCALE)
    (1 'INVALID_TRANSFORM)
    (2 'INVALID_SIZE)
    (3 'INVALID_OFFSET)
    (4 'DEFUNCT_ROLE_OBJECT)))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_SURFACE) EVENT)
  (CASE EVENT
    (EVT-ENTER 0)
    (EVT-LEAVE 1)
    (EVT-PREFERRED_BUFFER_SCALE 2)
    (EVT-PREFERRED_BUFFER_TRANSFORM 3)))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_SURFACE) OPCODE)
  (NTH OPCODE
       '(REQ-DESTROY REQ-ATTACH REQ-DAMAGE REQ-FRAME REQ-SET_OPAQUE_REGION
         REQ-SET_INPUT_REGION REQ-COMMIT REQ-SET_BUFFER_TRANSFORM
         REQ-SET_BUFFER_SCALE REQ-DAMAGE_BUFFER REQ-OFFSET)))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_SURFACE) OPCODE)
  (NTH OPCODE
       '(NIL (OBJECT INT INT) (INT INT INT INT) (NEW_ID) (OBJECT) (OBJECT) NIL
         ((ENUM (WL_OUTPUT TRANSFORM))) (INT) (INT INT INT INT) (INT INT))))

(DEFPACKAGE :WL/WL_SEAT
  (:USE :CL :WL)
  (:EXPORT WL_SEAT
           REQ-GET_POINTER
           REQ-GET_KEYBOARD
           REQ-GET_TOUCH
           REQ-RELEASE
           EVT-CAPABILITIES
           EVT-NAME))

(IN-PACKAGE :WL/WL_SEAT)

(DEFCLASS WL_SEAT (WL-OBJECT) ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "group of input devices

A seat is a group of keyboards, pointer and touch devices. This
      object is published as a global during start up, or when such a
      device is hot plugged.  A seat typically has a pointer and
      maintains a keyboard focus and a pointer focus.
"))

(DEFMETHOD EVT-CAPABILITIES ((OBJ WL_SEAT) STREAM CAPABILITIES)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-CAPABILITIES)))
    ";; seat capabilities changed

This is emitted whenever a seat gains or loses the pointer,
	keyboard or touch capabilities.  The argument is a capability
	enum containing the complete set of capabilities this seat has.

	When the pointer capability is added, a client may create a
	wl_pointer object using the wl_seat.get_pointer request. This object
	will receive pointer events until the capability is removed in the
	future.

	When the pointer capability is removed, a client should destroy the
	wl_pointer objects associated with the seat where the capability was
	removed, using the wl_pointer.release request. No further pointer
	events will be received on these objects.

	In some compositors, if a seat regains the pointer capability and a
	client has a previously obtained wl_pointer object of version 4 or
	less, that object may start sending pointer events again. This
	behavior is considered a misinterpretation of the intended behavior
	and must not be relied upon by the client. wl_pointer objects of
	version 5 or later must not send events if created before the most
	recent event notifying the client of an added pointer capability.

	The above behavior also applies to wl_keyboard and wl_touch with the
	keyboard and touch capabilities, respectively.

Arguments:
capabilities::enum: capabilities of the seat
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-NAME ((OBJ WL_SEAT) STREAM NAME)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-NAME)))
    ";; unique identifier for this seat

In a multi-seat configuration the seat name can be used by clients to
	help identify which physical devices the seat represents.

	The seat name is a UTF-8 string with no convention defined for its
	contents. Each name is unique among all wl_seat globals. The name is
	only guaranteed to be unique for the current compositor instance.

	The same seat names are used for all clients. Thus, the name can be
	shared across processes to refer to a specific wl_seat global.

	The name event is sent after binding to the seat global. This event is
	only sent once per seat object, and the name does not change over the
	lifetime of the wl_seat global.

	Compositors may re-use the same seat name if the wl_seat global is
	destroyed and re-created later.

Arguments:
name::string: seat identifier
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD REQ-GET_POINTER ((OBJ WL_SEAT) CLIENT ID)
  ";; return pointer object

The ID provided will be initialized to the wl_pointer interface
	for this seat.

	This request only takes effect if the seat has the pointer
	capability, or has had the pointer capability in the past.
	It is a protocol violation to issue this request on a seat that has
	never had the pointer capability. The missing_capability error will
	be sent in this case.

Arguments:
id::new_id: seat pointer
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-GET_KEYBOARD ((OBJ WL_SEAT) CLIENT ID)
  ";; return keyboard object

The ID provided will be initialized to the wl_keyboard interface
	for this seat.

	This request only takes effect if the seat has the keyboard
	capability, or has had the keyboard capability in the past.
	It is a protocol violation to issue this request on a seat that has
	never had the keyboard capability. The missing_capability error will
	be sent in this case.

Arguments:
id::new_id: seat keyboard
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-GET_TOUCH ((OBJ WL_SEAT) CLIENT ID)
  ";; return touch object

The ID provided will be initialized to the wl_touch interface
	for this seat.

	This request only takes effect if the seat has the touch
	capability, or has had the touch capability in the past.
	It is a protocol violation to issue this request on a seat that has
	never had the touch capability. The missing_capability error will
	be sent in this case.

Arguments:
id::new_id: seat touch interface
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-RELEASE ((OBJ WL_SEAT) CLIENT)
  ";; release the seat object

Using this request a client can tell the server that it is not going to
	use the seat object anymore.
"
  (ERROR "Unimplemented"))

(DEFMETHOD ENUM-CAPABILITY ((OBJ WL_SEAT) VALUE)
  ";; seat capability bitmask

This is a bitmask of capabilities this seat has; if a member is
	set, then it is present on the seat.
"
  (LET ((OPTIONS
         '((1 READ-FROM-STRING (NAME ENTRY)) (2 READ-FROM-STRING (NAME ENTRY))
           (4 READ-FROM-STRING (NAME ENTRY)))))
    (LOOP FOR (MASK NAME) IN OPTIONS
          WHEN (LOGBITP MASK VALUE)
          COLLECT NAME)))

(DEFMETHOD ENUM-ERROR ((OBJ WL_SEAT) VALUE)
  ";; wl_seat error values

These errors can be emitted in response to wl_seat requests.
"
  (CASE VALUE (0 'MISSING_CAPABILITY)))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_SEAT) EVENT)
  (CASE EVENT (EVT-CAPABILITIES 0) (EVT-NAME 1)))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_SEAT) OPCODE)
  (NTH OPCODE '(REQ-GET_POINTER REQ-GET_KEYBOARD REQ-GET_TOUCH REQ-RELEASE)))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_SEAT) OPCODE)
  (NTH OPCODE '((NEW_ID) (NEW_ID) (NEW_ID) NIL)))

(DEFPACKAGE :WL/WL_POINTER
  (:USE :CL :WL)
  (:EXPORT WL_POINTER
           REQ-SET_CURSOR
           REQ-RELEASE
           EVT-ENTER
           EVT-LEAVE
           EVT-MOTION
           EVT-BUTTON
           EVT-AXIS
           EVT-FRAME
           EVT-AXIS_SOURCE
           EVT-AXIS_STOP
           EVT-AXIS_DISCRETE
           EVT-AXIS_VALUE120
           EVT-AXIS_RELATIVE_DIRECTION))

(IN-PACKAGE :WL/WL_POINTER)

(DEFCLASS WL_POINTER (WL-OBJECT) ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "pointer input device

The wl_pointer interface represents one or more input devices,
      such as mice, which control the pointer location and pointer_focus
      of a seat.

      The wl_pointer interface generates motion, enter and leave
      events for the surfaces that the pointer is located over,
      and button and axis events for button presses, button releases
      and scrolling.
"))

(DEFMETHOD EVT-ENTER
           ((OBJ WL_POINTER) STREAM SERIAL SURFACE SURFACE_X SURFACE_Y)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-ENTER)))
    ";; enter event

Notification that this seat's pointer is focused on a certain
	surface.

	When a seat's focus enters a surface, the pointer image
	is undefined and a client should respond to this event by setting
	an appropriate pointer image with the set_cursor request.

Arguments:
serial::uint: serial number of the enter event
surface::object: surface entered by the pointer
surface_x::fixed: surface-local x coordinate
surface_y::fixed: surface-local y coordinate
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-LEAVE ((OBJ WL_POINTER) STREAM SERIAL SURFACE)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-LEAVE)))
    ";; leave event

Notification that this seat's pointer is no longer focused on
	a certain surface.

	The leave notification is sent before the enter notification
	for the new focus.

Arguments:
serial::uint: serial number of the leave event
surface::object: surface left by the pointer
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-MOTION ((OBJ WL_POINTER) STREAM TIME SURFACE_X SURFACE_Y)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-MOTION)))
    ";; pointer motion event

Notification of pointer location change. The arguments
	surface_x and surface_y are the location relative to the
	focused surface.

Arguments:
time::uint: timestamp with millisecond granularity
surface_x::fixed: surface-local x coordinate
surface_y::fixed: surface-local y coordinate
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-BUTTON ((OBJ WL_POINTER) STREAM SERIAL TIME BUTTON STATE)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-BUTTON)))
    ";; pointer button event

Mouse button click and release notifications.

	The location of the click is given by the last motion or
	enter event.
	The time argument is a timestamp with millisecond
	granularity, with an undefined base.

	The button is a button code as defined in the Linux kernel's
	linux/input-event-codes.h header file, e.g. BTN_LEFT.

	Any 16-bit button code value is reserved for future additions to the
	kernel's event code list. All other button codes above 0xFFFF are
	currently undefined but may be used in future versions of this
	protocol.

Arguments:
serial::uint: serial number of the button event
time::uint: timestamp with millisecond granularity
button::uint: button that produced the event
state::enum: physical state of the button
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-AXIS ((OBJ WL_POINTER) STREAM TIME AXIS VALUE)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-AXIS)))
    ";; axis event

Scroll and other axis notifications.

	For scroll events (vertical and horizontal scroll axes), the
	value parameter is the length of a vector along the specified
	axis in a coordinate space identical to those of motion events,
	representing a relative movement along the specified axis.

	For devices that support movements non-parallel to axes multiple
	axis events will be emitted.

	When applicable, for example for touch pads, the server can
	choose to emit scroll events where the motion vector is
	equivalent to a motion event vector.

	When applicable, a client can transform its content relative to the
	scroll distance.

Arguments:
time::uint: timestamp with millisecond granularity
axis::enum: axis type
value::fixed: length of vector in surface-local coordinate space
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-FRAME ((OBJ WL_POINTER) STREAM)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-FRAME)))
    ";; end of a pointer event sequence

Indicates the end of a set of events that logically belong together.
	A client is expected to accumulate the data in all events within the
	frame before proceeding.

	All wl_pointer events before a wl_pointer.frame event belong
	logically together. For example, in a diagonal scroll motion the
	compositor will send an optional wl_pointer.axis_source event, two
	wl_pointer.axis events (horizontal and vertical) and finally a
	wl_pointer.frame event. The client may use this information to
	calculate a diagonal vector for scrolling.

	When multiple wl_pointer.axis events occur within the same frame,
	the motion vector is the combined motion of all events.
	When a wl_pointer.axis and a wl_pointer.axis_stop event occur within
	the same frame, this indicates that axis movement in one axis has
	stopped but continues in the other axis.
	When multiple wl_pointer.axis_stop events occur within the same
	frame, this indicates that these axes stopped in the same instance.

	A wl_pointer.frame event is sent for every logical event group,
	even if the group only contains a single wl_pointer event.
	Specifically, a client may get a sequence: motion, frame, button,
	frame, axis, frame, axis_stop, frame.

	The wl_pointer.enter and wl_pointer.leave events are logical events
	generated by the compositor and not the hardware. These events are
	also grouped by a wl_pointer.frame. When a pointer moves from one
	surface to another, a compositor should group the
	wl_pointer.leave event within the same wl_pointer.frame.
	However, a client must not rely on wl_pointer.leave and
	wl_pointer.enter being in the same wl_pointer.frame.
	Compositor-specific policies may require the wl_pointer.leave and
	wl_pointer.enter event being split across multiple wl_pointer.frame
	groups.
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-AXIS_SOURCE ((OBJ WL_POINTER) STREAM AXIS_SOURCE)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-AXIS_SOURCE)))
    ";; axis source event

Source information for scroll and other axes.

	This event does not occur on its own. It is sent before a
	wl_pointer.frame event and carries the source information for
	all events within that frame.

	The source specifies how this event was generated. If the source is
	wl_pointer.axis_source.finger, a wl_pointer.axis_stop event will be
	sent when the user lifts the finger off the device.

	If the source is wl_pointer.axis_source.wheel,
	wl_pointer.axis_source.wheel_tilt or
	wl_pointer.axis_source.continuous, a wl_pointer.axis_stop event may
	or may not be sent. Whether a compositor sends an axis_stop event
	for these sources is hardware-specific and implementation-dependent;
	clients must not rely on receiving an axis_stop event for these
	scroll sources and should treat scroll sequences from these scroll
	sources as unterminated by default.

	This event is optional. If the source is unknown for a particular
	axis event sequence, no event is sent.
	Only one wl_pointer.axis_source event is permitted per frame.

	The order of wl_pointer.axis_discrete and wl_pointer.axis_source is
	not guaranteed.

Arguments:
axis_source::enum: source of the axis event
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-AXIS_STOP ((OBJ WL_POINTER) STREAM TIME AXIS)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-AXIS_STOP)))
    ";; axis stop event

Stop notification for scroll and other axes.

	For some wl_pointer.axis_source types, a wl_pointer.axis_stop event
	is sent to notify a client that the axis sequence has terminated.
	This enables the client to implement kinetic scrolling.
	See the wl_pointer.axis_source documentation for information on when
	this event may be generated.

	Any wl_pointer.axis events with the same axis_source after this
	event should be considered as the start of a new axis motion.

	The timestamp is to be interpreted identical to the timestamp in the
	wl_pointer.axis event. The timestamp value may be the same as a
	preceding wl_pointer.axis event.

Arguments:
time::uint: timestamp with millisecond granularity
axis::enum: the axis stopped with this event
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-AXIS_DISCRETE ((OBJ WL_POINTER) STREAM AXIS DISCRETE)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-AXIS_DISCRETE)))
    ";; axis click event

Discrete step information for scroll and other axes.

	This event carries the axis value of the wl_pointer.axis event in
	discrete steps (e.g. mouse wheel clicks).

	This event is deprecated with wl_pointer version 8 - this event is not
	sent to clients supporting version 8 or later.

	This event does not occur on its own, it is coupled with a
	wl_pointer.axis event that represents this axis value on a
	continuous scale. The protocol guarantees that each axis_discrete
	event is always followed by exactly one axis event with the same
	axis number within the same wl_pointer.frame. Note that the protocol
	allows for other events to occur between the axis_discrete and
	its coupled axis event, including other axis_discrete or axis
	events. A wl_pointer.frame must not contain more than one axis_discrete
	event per axis type.

	This event is optional; continuous scrolling devices
	like two-finger scrolling on touchpads do not have discrete
	steps and do not generate this event.

	The discrete value carries the directional information. e.g. a value
	of -2 is two steps towards the negative direction of this axis.

	The axis number is identical to the axis number in the associated
	axis event.

	The order of wl_pointer.axis_discrete and wl_pointer.axis_source is
	not guaranteed.

Arguments:
axis::enum: axis type
discrete::int: number of steps
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-AXIS_VALUE120 ((OBJ WL_POINTER) STREAM AXIS VALUE120)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-AXIS_VALUE120)))
    ";; axis high-resolution scroll event

Discrete high-resolution scroll information.

	This event carries high-resolution wheel scroll information,
	with each multiple of 120 representing one logical scroll step
	(a wheel detent). For example, an axis_value120 of 30 is one quarter of
	a logical scroll step in the positive direction, a value120 of
	-240 are two logical scroll steps in the negative direction within the
	same hardware event.
	Clients that rely on discrete scrolling should accumulate the
	value120 to multiples of 120 before processing the event.

	The value120 must not be zero.

	This event replaces the wl_pointer.axis_discrete event in clients
	supporting wl_pointer version 8 or later.

	Where a wl_pointer.axis_source event occurs in the same
	wl_pointer.frame, the axis source applies to this event.

	The order of wl_pointer.axis_value120 and wl_pointer.axis_source is
	not guaranteed.

Arguments:
axis::enum: axis type
value120::int: scroll distance as fraction of 120
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-AXIS_RELATIVE_DIRECTION ((OBJ WL_POINTER) STREAM AXIS DIRECTION)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-AXIS_RELATIVE_DIRECTION)))
    ";; axis relative physical direction event

Relative directional information of the entity causing the axis
	motion.

	For a wl_pointer.axis event, the wl_pointer.axis_relative_direction
	event specifies the movement direction of the entity causing the
	wl_pointer.axis event. For example:
	- if a user's fingers on a touchpad move down and this
	  causes a wl_pointer.axis vertical_scroll down event, the physical
	  direction is 'identical'
	- if a user's fingers on a touchpad move down and this causes a
	  wl_pointer.axis vertical_scroll up scroll up event ('natural
	  scrolling'), the physical direction is 'inverted'.

	A client may use this information to adjust scroll motion of
	components. Specifically, enabling natural scrolling causes the
	content to change direction compared to traditional scrolling.
	Some widgets like volume control sliders should usually match the
	physical direction regardless of whether natural scrolling is
	active. This event enables clients to match the scroll direction of
	a widget to the physical direction.

	This event does not occur on its own, it is coupled with a
	wl_pointer.axis event that represents this axis value.
	The protocol guarantees that each axis_relative_direction event is
	always followed by exactly one axis event with the same
	axis number within the same wl_pointer.frame. Note that the protocol
	allows for other events to occur between the axis_relative_direction
	and its coupled axis event.

	The axis number is identical to the axis number in the associated
	axis event.

	The order of wl_pointer.axis_relative_direction,
	wl_pointer.axis_discrete and wl_pointer.axis_source is not
	guaranteed.

Arguments:
axis::enum: axis type
direction::enum: physical direction relative to axis motion
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD REQ-SET_CURSOR
           ((OBJ WL_POINTER) CLIENT SERIAL SURFACE HOTSPOT_X HOTSPOT_Y)
  ";; set the pointer surface

Set the pointer surface, i.e., the surface that contains the
	pointer image (cursor). This request gives the surface the role
	of a cursor. If the surface already has another role, it raises
	a protocol error.

	The cursor actually changes only if the pointer
	focus for this device is one of the requesting client's surfaces
	or the surface parameter is the current pointer surface. If
	there was a previous surface set with this request it is
	replaced. If surface is NULL, the pointer image is hidden.

	The parameters hotspot_x and hotspot_y define the position of
	the pointer surface relative to the pointer location. Its
	top-left corner is always at (x, y) - (hotspot_x, hotspot_y),
	where (x, y) are the coordinates of the pointer location, in
	surface-local coordinates.

	On surface.attach requests to the pointer surface, hotspot_x
	and hotspot_y are decremented by the x and y parameters
	passed to the request. Attach must be confirmed by
	wl_surface.commit as usual.

	The hotspot can also be updated by passing the currently set
	pointer surface to this request with new values for hotspot_x
	and hotspot_y.

	The input region is ignored for wl_surfaces with the role of
	a cursor. When the use as a cursor ends, the wl_surface is
	unmapped.

	The serial parameter must match the latest wl_pointer.enter
	serial number sent to the client. Otherwise the request will be
	ignored.

Arguments:
serial::uint: serial number of the enter event
surface::object: pointer surface
hotspot_x::int: surface-local x coordinate
hotspot_y::int: surface-local y coordinate
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-RELEASE ((OBJ WL_POINTER) CLIENT)
  ";; release the pointer object

Using this request a client can tell the server that it is not going to
	use the pointer object anymore.

	This request destroys the pointer proxy object, so clients must not call
	wl_pointer_destroy() after using this request.
"
  (ERROR "Unimplemented"))

(DEFMETHOD ENUM-ERROR ((OBJ WL_POINTER) VALUE) ";; " (CASE VALUE (0 'ROLE)))

(DEFMETHOD ENUM-BUTTON_STATE ((OBJ WL_POINTER) VALUE)
  ";; physical button state

Describes the physical state of a button that produced the button
	event.
"
  (CASE VALUE (0 'RELEASED) (1 'PRESSED)))

(DEFMETHOD ENUM-AXIS ((OBJ WL_POINTER) VALUE)
  ";; axis types

Describes the axis types of scroll events.
"
  (CASE VALUE (0 'VERTICAL_SCROLL) (1 'HORIZONTAL_SCROLL)))

(DEFMETHOD ENUM-AXIS_SOURCE ((OBJ WL_POINTER) VALUE)
  ";; axis source types

Describes the source types for axis events. This indicates to the
	client how an axis event was physically generated; a client may
	adjust the user interface accordingly. For example, scroll events
	from a \"finger\" source may be in a smooth coordinate space with
	kinetic scrolling whereas a \"wheel\" source may be in discrete steps
	of a number of lines.

	The \"continuous\" axis source is a device generating events in a
	continuous coordinate space, but using something other than a
	finger. One example for this source is button-based scrolling where
	the vertical motion of a device is converted to scroll events while
	a button is held down.

	The \"wheel tilt\" axis source indicates that the actual device is a
	wheel but the scroll event is not caused by a rotation but a
	(usually sideways) tilt of the wheel.
"
  (CASE VALUE (0 'WHEEL) (1 'FINGER) (2 'CONTINUOUS) (3 'WHEEL_TILT)))

(DEFMETHOD ENUM-AXIS_RELATIVE_DIRECTION ((OBJ WL_POINTER) VALUE)
  ";; axis relative direction

This specifies the direction of the physical motion that caused a
	wl_pointer.axis event, relative to the wl_pointer.axis direction.
"
  (CASE VALUE (0 'IDENTICAL) (1 'INVERTED)))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_POINTER) EVENT)
  (CASE EVENT
    (EVT-ENTER 0)
    (EVT-LEAVE 1)
    (EVT-MOTION 2)
    (EVT-BUTTON 3)
    (EVT-AXIS 4)
    (EVT-FRAME 5)
    (EVT-AXIS_SOURCE 6)
    (EVT-AXIS_STOP 7)
    (EVT-AXIS_DISCRETE 8)
    (EVT-AXIS_VALUE120 9)
    (EVT-AXIS_RELATIVE_DIRECTION 10)))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_POINTER) OPCODE)
  (NTH OPCODE '(REQ-SET_CURSOR REQ-RELEASE)))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_POINTER) OPCODE)
  (NTH OPCODE '((UINT OBJECT INT INT) NIL)))

(DEFPACKAGE :WL/WL_KEYBOARD
  (:USE :CL :WL)
  (:EXPORT WL_KEYBOARD
           REQ-RELEASE
           EVT-KEYMAP
           EVT-ENTER
           EVT-LEAVE
           EVT-KEY
           EVT-MODIFIERS
           EVT-REPEAT_INFO))

(IN-PACKAGE :WL/WL_KEYBOARD)

(DEFCLASS WL_KEYBOARD (WL-OBJECT) ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "keyboard input device

The wl_keyboard interface represents one or more keyboards
      associated with a seat.
"))

(DEFMETHOD EVT-KEYMAP ((OBJ WL_KEYBOARD) STREAM FORMAT FD SIZE)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-KEYMAP)))
    ";; keyboard mapping

This event provides a file descriptor to the client which can be
	memory-mapped in read-only mode to provide a keyboard mapping
	description.

	From version 7 onwards, the fd must be mapped with MAP_PRIVATE by
	the recipient, as MAP_SHARED may fail.

Arguments:
format::enum: keymap format
fd::fd: keymap file descriptor
size::uint: keymap size, in bytes
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-ENTER ((OBJ WL_KEYBOARD) STREAM SERIAL SURFACE KEYS)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-ENTER)))
    ";; enter event

Notification that this seat's keyboard focus is on a certain
	surface.

	The compositor must send the wl_keyboard.modifiers event after this
	event.

Arguments:
serial::uint: serial number of the enter event
surface::object: surface gaining keyboard focus
keys::array: the currently pressed keys
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-LEAVE ((OBJ WL_KEYBOARD) STREAM SERIAL SURFACE)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-LEAVE)))
    ";; leave event

Notification that this seat's keyboard focus is no longer on
	a certain surface.

	The leave notification is sent before the enter notification
	for the new focus.

	After this event client must assume that all keys, including modifiers,
	are lifted and also it must stop key repeating if there's some going on.

Arguments:
serial::uint: serial number of the leave event
surface::object: surface that lost keyboard focus
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-KEY ((OBJ WL_KEYBOARD) STREAM SERIAL TIME KEY STATE)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-KEY)))
    ";; key event

A key was pressed or released.
	The time argument is a timestamp with millisecond
	granularity, with an undefined base.

	The key is a platform-specific key code that can be interpreted
	by feeding it to the keyboard mapping (see the keymap event).

	If this event produces a change in modifiers, then the resulting
	wl_keyboard.modifiers event must be sent after this event.

Arguments:
serial::uint: serial number of the key event
time::uint: timestamp with millisecond granularity
key::uint: key that produced the event
state::enum: physical state of the key
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-MODIFIERS
           ((OBJ WL_KEYBOARD) STREAM SERIAL MODS_DEPRESSED MODS_LATCHED
            MODS_LOCKED GROUP)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-MODIFIERS)))
    ";; modifier and group state

Notifies clients that the modifier and/or group state has
	changed, and it should update its local state.

Arguments:
serial::uint: serial number of the modifiers event
mods_depressed::uint: depressed modifiers
mods_latched::uint: latched modifiers
mods_locked::uint: locked modifiers
group::uint: keyboard layout
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-REPEAT_INFO ((OBJ WL_KEYBOARD) STREAM RATE DELAY)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-REPEAT_INFO)))
    ";; repeat rate and delay

Informs the client about the keyboard's repeat rate and delay.

	This event is sent as soon as the wl_keyboard object has been created,
	and is guaranteed to be received by the client before any key press
	event.

	Negative values for either rate or delay are illegal. A rate of zero
	will disable any repeating (regardless of the value of delay).

	This event can be sent later on as well with a new value if necessary,
	so clients should continue listening for the event past the creation
	of wl_keyboard.

Arguments:
rate::int: the rate of repeating keys in characters per second
delay::int: delay in milliseconds since key down until repeating starts
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD REQ-RELEASE ((OBJ WL_KEYBOARD) CLIENT)
  ";; release the keyboard object

NIL
"
  (ERROR "Unimplemented"))

(DEFMETHOD ENUM-KEYMAP_FORMAT ((OBJ WL_KEYBOARD) VALUE)
  ";; keyboard mapping format

This specifies the format of the keymap provided to the
	client with the wl_keyboard.keymap event.
"
  (CASE VALUE (0 'NO_KEYMAP) (1 'XKB_V1)))

(DEFMETHOD ENUM-KEY_STATE ((OBJ WL_KEYBOARD) VALUE)
  ";; physical key state

Describes the physical state of a key that produced the key event.
"
  (CASE VALUE (0 'RELEASED) (1 'PRESSED)))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_KEYBOARD) EVENT)
  (CASE EVENT
    (EVT-KEYMAP 0)
    (EVT-ENTER 1)
    (EVT-LEAVE 2)
    (EVT-KEY 3)
    (EVT-MODIFIERS 4)
    (EVT-REPEAT_INFO 5)))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_KEYBOARD) OPCODE)
  (NTH OPCODE '(REQ-RELEASE)))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_KEYBOARD) OPCODE) (NTH OPCODE '(NIL)))

(DEFPACKAGE :WL/WL_TOUCH
  (:USE :CL :WL)
  (:EXPORT WL_TOUCH
           REQ-RELEASE
           EVT-DOWN
           EVT-UP
           EVT-MOTION
           EVT-FRAME
           EVT-CANCEL
           EVT-SHAPE
           EVT-ORIENTATION))

(IN-PACKAGE :WL/WL_TOUCH)

(DEFCLASS WL_TOUCH (WL-OBJECT) ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "touchscreen input device

The wl_touch interface represents a touchscreen
      associated with a seat.

      Touch interactions can consist of one or more contacts.
      For each contact, a series of events is generated, starting
      with a down event, followed by zero or more motion events,
      and ending with an up event. Events relating to the same
      contact point can be identified by the ID of the sequence.
"))

(DEFMETHOD EVT-DOWN ((OBJ WL_TOUCH) STREAM SERIAL TIME SURFACE ID X Y)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-DOWN)))
    ";; touch down event and beginning of a touch sequence

A new touch point has appeared on the surface. This touch point is
	assigned a unique ID. Future events from this touch point reference
	this ID. The ID ceases to be valid after a touch up event and may be
	reused in the future.

Arguments:
serial::uint: serial number of the touch down event
time::uint: timestamp with millisecond granularity
surface::object: surface touched
id::int: the unique ID of this touch point
x::fixed: surface-local x coordinate
y::fixed: surface-local y coordinate
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-UP ((OBJ WL_TOUCH) STREAM SERIAL TIME ID)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-UP)))
    ";; end of a touch event sequence

The touch point has disappeared. No further events will be sent for
	this touch point and the touch point's ID is released and may be
	reused in a future touch down event.

Arguments:
serial::uint: serial number of the touch up event
time::uint: timestamp with millisecond granularity
id::int: the unique ID of this touch point
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-MOTION ((OBJ WL_TOUCH) STREAM TIME ID X Y)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-MOTION)))
    ";; update of touch point coordinates

A touch point has changed coordinates.

Arguments:
time::uint: timestamp with millisecond granularity
id::int: the unique ID of this touch point
x::fixed: surface-local x coordinate
y::fixed: surface-local y coordinate
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-FRAME ((OBJ WL_TOUCH) STREAM)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-FRAME)))
    ";; end of touch frame event

Indicates the end of a set of events that logically belong together.
	A client is expected to accumulate the data in all events within the
	frame before proceeding.

	A wl_touch.frame terminates at least one event but otherwise no
	guarantee is provided about the set of events within a frame. A client
	must assume that any state not updated in a frame is unchanged from the
	previously known state.
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-CANCEL ((OBJ WL_TOUCH) STREAM)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-CANCEL)))
    ";; touch session cancelled

Sent if the compositor decides the touch stream is a global
	gesture. No further events are sent to the clients from that
	particular gesture. Touch cancellation applies to all touch points
	currently active on this client's surface. The client is
	responsible for finalizing the touch points, future touch points on
	this surface may reuse the touch point ID.
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-SHAPE ((OBJ WL_TOUCH) STREAM ID MAJOR MINOR)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-SHAPE)))
    ";; update shape of touch point

Sent when a touchpoint has changed its shape.

	This event does not occur on its own. It is sent before a
	wl_touch.frame event and carries the new shape information for
	any previously reported, or new touch points of that frame.

	Other events describing the touch point such as wl_touch.down,
	wl_touch.motion or wl_touch.orientation may be sent within the
	same wl_touch.frame. A client should treat these events as a single
	logical touch point update. The order of wl_touch.shape,
	wl_touch.orientation and wl_touch.motion is not guaranteed.
	A wl_touch.down event is guaranteed to occur before the first
	wl_touch.shape event for this touch ID but both events may occur within
	the same wl_touch.frame.

	A touchpoint shape is approximated by an ellipse through the major and
	minor axis length. The major axis length describes the longer diameter
	of the ellipse, while the minor axis length describes the shorter
	diameter. Major and minor are orthogonal and both are specified in
	surface-local coordinates. The center of the ellipse is always at the
	touchpoint location as reported by wl_touch.down or wl_touch.move.

	This event is only sent by the compositor if the touch device supports
	shape reports. The client has to make reasonable assumptions about the
	shape if it did not receive this event.

Arguments:
id::int: the unique ID of this touch point
major::fixed: length of the major axis in surface-local coordinates
minor::fixed: length of the minor axis in surface-local coordinates
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-ORIENTATION ((OBJ WL_TOUCH) STREAM ID ORIENTATION)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-ORIENTATION)))
    ";; update orientation of touch point

Sent when a touchpoint has changed its orientation.

	This event does not occur on its own. It is sent before a
	wl_touch.frame event and carries the new shape information for
	any previously reported, or new touch points of that frame.

	Other events describing the touch point such as wl_touch.down,
	wl_touch.motion or wl_touch.shape may be sent within the
	same wl_touch.frame. A client should treat these events as a single
	logical touch point update. The order of wl_touch.shape,
	wl_touch.orientation and wl_touch.motion is not guaranteed.
	A wl_touch.down event is guaranteed to occur before the first
	wl_touch.orientation event for this touch ID but both events may occur
	within the same wl_touch.frame.

	The orientation describes the clockwise angle of a touchpoint's major
	axis to the positive surface y-axis and is normalized to the -180 to
	+180 degree range. The granularity of orientation depends on the touch
	device, some devices only support binary rotation values between 0 and
	90 degrees.

	This event is only sent by the compositor if the touch device supports
	orientation reports.

Arguments:
id::int: the unique ID of this touch point
orientation::fixed: angle between major axis and positive surface y-axis in degrees
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD REQ-RELEASE ((OBJ WL_TOUCH) CLIENT)
  ";; release the touch object

NIL
"
  (ERROR "Unimplemented"))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_TOUCH) EVENT)
  (CASE EVENT
    (EVT-DOWN 0)
    (EVT-UP 1)
    (EVT-MOTION 2)
    (EVT-FRAME 3)
    (EVT-CANCEL 4)
    (EVT-SHAPE 5)
    (EVT-ORIENTATION 6)))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_TOUCH) OPCODE)
  (NTH OPCODE '(REQ-RELEASE)))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_TOUCH) OPCODE) (NTH OPCODE '(NIL)))

(DEFPACKAGE :WL/WL_OUTPUT
  (:USE :CL :WL)
  (:EXPORT WL_OUTPUT
           REQ-RELEASE
           EVT-GEOMETRY
           EVT-MODE
           EVT-DONE
           EVT-SCALE
           EVT-NAME
           EVT-DESCRIPTION))

(IN-PACKAGE :WL/WL_OUTPUT)

(DEFCLASS WL_OUTPUT (WL-OBJECT) ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "compositor output region

An output describes part of the compositor geometry.  The
      compositor works in the 'compositor coordinate system' and an
      output corresponds to a rectangular area in that space that is
      actually visible.  This typically corresponds to a monitor that
      displays part of the compositor space.  This object is published
      as global during start up, or when a monitor is hotplugged.
"))

(DEFMETHOD EVT-GEOMETRY
           ((OBJ WL_OUTPUT) STREAM X Y PHYSICAL_WIDTH PHYSICAL_HEIGHT SUBPIXEL
            MAKE MODEL TRANSFORM)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-GEOMETRY)))
    ";; properties of the output

The geometry event describes geometric properties of the output.
	The event is sent when binding to the output object and whenever
	any of the properties change.

	The physical size can be set to zero if it doesn't make sense for this
	output (e.g. for projectors or virtual outputs).

	The geometry event will be followed by a done event (starting from
	version 2).

	Note: wl_output only advertises partial information about the output
	position and identification. Some compositors, for instance those not
	implementing a desktop-style output layout or those exposing virtual
	outputs, might fake this information. Instead of using x and y, clients
	should use xdg_output.logical_position. Instead of using make and model,
	clients should use name and description.

Arguments:
x::int: x position within the global compositor space
y::int: y position within the global compositor space
physical_width::int: width in millimeters of the output
physical_height::int: height in millimeters of the output
subpixel::enum: subpixel orientation of the output
make::string: textual description of the manufacturer
model::string: textual description of the model
transform::enum: transform that maps framebuffer to output
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-MODE ((OBJ WL_OUTPUT) STREAM FLAGS WIDTH HEIGHT REFRESH)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-MODE)))
    ";; advertise available modes for the output

The mode event describes an available mode for the output.

	The event is sent when binding to the output object and there
	will always be one mode, the current mode.  The event is sent
	again if an output changes mode, for the mode that is now
	current.  In other words, the current mode is always the last
	mode that was received with the current flag set.

	Non-current modes are deprecated. A compositor can decide to only
	advertise the current mode and never send other modes. Clients
	should not rely on non-current modes.

	The size of a mode is given in physical hardware units of
	the output device. This is not necessarily the same as
	the output size in the global compositor space. For instance,
	the output may be scaled, as described in wl_output.scale,
	or transformed, as described in wl_output.transform. Clients
	willing to retrieve the output size in the global compositor
	space should use xdg_output.logical_size instead.

	The vertical refresh rate can be set to zero if it doesn't make
	sense for this output (e.g. for virtual outputs).

	The mode event will be followed by a done event (starting from
	version 2).

	Clients should not use the refresh rate to schedule frames. Instead,
	they should use the wl_surface.frame event or the presentation-time
	protocol.

	Note: this information is not always meaningful for all outputs. Some
	compositors, such as those exposing virtual outputs, might fake the
	refresh rate or the size.

Arguments:
flags::enum: bitfield of mode flags
width::int: width of the mode in hardware units
height::int: height of the mode in hardware units
refresh::int: vertical refresh rate in mHz
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-DONE ((OBJ WL_OUTPUT) STREAM)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-DONE)))
    ";; sent all information about output

This event is sent after all other properties have been
	sent after binding to the output object and after any
	other property changes done after that. This allows
	changes to the output properties to be seen as
	atomic, even if they happen via multiple events.
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-SCALE ((OBJ WL_OUTPUT) STREAM FACTOR)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-SCALE)))
    ";; output scaling properties

This event contains scaling geometry information
	that is not in the geometry event. It may be sent after
	binding the output object or if the output scale changes
	later. If it is not sent, the client should assume a
	scale of 1.

	A scale larger than 1 means that the compositor will
	automatically scale surface buffers by this amount
	when rendering. This is used for very high resolution
	displays where applications rendering at the native
	resolution would be too small to be legible.

	It is intended that scaling aware clients track the
	current output of a surface, and if it is on a scaled
	output it should use wl_surface.set_buffer_scale with
	the scale of the output. That way the compositor can
	avoid scaling the surface, and the client can supply
	a higher detail image.

	The scale event will be followed by a done event.

Arguments:
factor::int: scaling factor of output
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-NAME ((OBJ WL_OUTPUT) STREAM NAME)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-NAME)))
    ";; name of this output

Many compositors will assign user-friendly names to their outputs, show
	them to the user, allow the user to refer to an output, etc. The client
	may wish to know this name as well to offer the user similar behaviors.

	The name is a UTF-8 string with no convention defined for its contents.
	Each name is unique among all wl_output globals. The name is only
	guaranteed to be unique for the compositor instance.

	The same output name is used for all clients for a given wl_output
	global. Thus, the name can be shared across processes to refer to a
	specific wl_output global.

	The name is not guaranteed to be persistent across sessions, thus cannot
	be used to reliably identify an output in e.g. configuration files.

	Examples of names include 'HDMI-A-1', 'WL-1', 'X11-1', etc. However, do
	not assume that the name is a reflection of an underlying DRM connector,
	X11 connection, etc.

	The name event is sent after binding the output object. This event is
	only sent once per output object, and the name does not change over the
	lifetime of the wl_output global.

	Compositors may re-use the same output name if the wl_output global is
	destroyed and re-created later. Compositors should avoid re-using the
	same name if possible.

	The name event will be followed by a done event.

Arguments:
name::string: output name
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD EVT-DESCRIPTION ((OBJ WL_OUTPUT) STREAM DESCRIPTION)
  (LET ((OPCODE (MATCH-EVENT-OPCODE OBJ EVT-DESCRIPTION)))
    ";; human-readable description of this output

Many compositors can produce human-readable descriptions of their
	outputs. The client may wish to know this description as well, e.g. for
	output selection purposes.

	The description is a UTF-8 string with no convention defined for its
	contents. The description is not guaranteed to be unique among all
	wl_output globals. Examples might include 'Foocorp 11\" Display' or
	'Virtual X11 output via :1'.

	The description event is sent after binding the output object and
	whenever the description changes. The description is optional, and may
	not be sent at all.

	The description event will be followed by a done event.

Arguments:
description::string: output description
"
    (ERROR "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(DEFMETHOD REQ-RELEASE ((OBJ WL_OUTPUT) CLIENT)
  ";; release the output object

Using this request a client can tell the server that it is not going to
	use the output object anymore.
"
  (ERROR "Unimplemented"))

(DEFMETHOD ENUM-SUBPIXEL ((OBJ WL_OUTPUT) VALUE)
  ";; subpixel geometry information

This enumeration describes how the physical
	pixels on an output are laid out.
"
  (CASE VALUE
    (0 'UNKNOWN)
    (1 'NONE)
    (2 'HORIZONTAL_RGB)
    (3 'HORIZONTAL_BGR)
    (4 'VERTICAL_RGB)
    (5 'VERTICAL_BGR)))

(DEFMETHOD ENUM-TRANSFORM ((OBJ WL_OUTPUT) VALUE)
  ";; transform from framebuffer to output

This describes the transform that a compositor will apply to a
	surface to compensate for the rotation or mirroring of an
	output device.

	The flipped values correspond to an initial flip around a
	vertical axis followed by rotation.

	The purpose is mainly to allow clients to render accordingly and
	tell the compositor, so that for fullscreen surfaces, the
	compositor will still be able to scan out directly from client
	surfaces.
"
  (CASE VALUE
    (0 'NORMAL)
    (1 '90)
    (2 '180)
    (3 '270)
    (4 'FLIPPED)
    (5 'FLIPPED_90)
    (6 'FLIPPED_180)
    (7 'FLIPPED_270)))

(DEFMETHOD ENUM-MODE ((OBJ WL_OUTPUT) VALUE)
  ";; mode information

These flags describe properties of an output mode.
	They are used in the flags bitfield of the mode event.
"
  (LET ((OPTIONS
         '((1 READ-FROM-STRING (NAME ENTRY))
           (2 READ-FROM-STRING (NAME ENTRY)))))
    (LOOP FOR (MASK NAME) IN OPTIONS
          WHEN (LOGBITP MASK VALUE)
          COLLECT NAME)))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_OUTPUT) EVENT)
  (CASE EVENT
    (EVT-GEOMETRY 0)
    (EVT-MODE 1)
    (EVT-DONE 2)
    (EVT-SCALE 3)
    (EVT-NAME 4)
    (EVT-DESCRIPTION 5)))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_OUTPUT) OPCODE)
  (NTH OPCODE '(REQ-RELEASE)))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_OUTPUT) OPCODE) (NTH OPCODE '(NIL)))

(DEFPACKAGE :WL/WL_REGION
  (:USE :CL :WL)
  (:EXPORT WL_REGION REQ-DESTROY REQ-ADD REQ-SUBTRACT))

(IN-PACKAGE :WL/WL_REGION)

(DEFCLASS WL_REGION (WL-OBJECT) ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "region interface

A region object describes an area.

      Region objects are used to describe the opaque and input
      regions of a surface.
"))

(DEFMETHOD REQ-DESTROY ((OBJ WL_REGION) CLIENT)
  ";; destroy region

Destroy the region.  This will invalidate the object ID.
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-ADD ((OBJ WL_REGION) CLIENT X Y WIDTH HEIGHT)
  ";; add rectangle to region

Add the specified rectangle to the region.

Arguments:
x::int: region-local x coordinate
y::int: region-local y coordinate
width::int: rectangle width
height::int: rectangle height
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-SUBTRACT ((OBJ WL_REGION) CLIENT X Y WIDTH HEIGHT)
  ";; subtract rectangle from region

Subtract the specified rectangle from the region.

Arguments:
x::int: region-local x coordinate
y::int: region-local y coordinate
width::int: rectangle width
height::int: rectangle height
"
  (ERROR "Unimplemented"))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_REGION) EVENT) (CASE EVENT))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_REGION) OPCODE)
  (NTH OPCODE '(REQ-DESTROY REQ-ADD REQ-SUBTRACT)))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_REGION) OPCODE)
  (NTH OPCODE '(NIL (INT INT INT INT) (INT INT INT INT))))

(DEFPACKAGE :WL/WL_SUBCOMPOSITOR
  (:USE :CL :WL)
  (:EXPORT WL_SUBCOMPOSITOR REQ-DESTROY REQ-GET_SUBSURFACE))

(IN-PACKAGE :WL/WL_SUBCOMPOSITOR)

(DEFCLASS WL_SUBCOMPOSITOR (WL-OBJECT)
          ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "sub-surface compositing

The global interface exposing sub-surface compositing capabilities.
      A wl_surface, that has sub-surfaces associated, is called the
      parent surface. Sub-surfaces can be arbitrarily nested and create
      a tree of sub-surfaces.

      The root surface in a tree of sub-surfaces is the main
      surface. The main surface cannot be a sub-surface, because
      sub-surfaces must always have a parent.

      A main surface with its sub-surfaces forms a (compound) window.
      For window management purposes, this set of wl_surface objects is
      to be considered as a single window, and it should also behave as
      such.

      The aim of sub-surfaces is to offload some of the compositing work
      within a window from clients to the compositor. A prime example is
      a video player with decorations and video in separate wl_surface
      objects. This should allow the compositor to pass YUV video buffer
      processing to dedicated overlay hardware when possible.
"))

(DEFMETHOD REQ-DESTROY ((OBJ WL_SUBCOMPOSITOR) CLIENT)
  ";; unbind from the subcompositor interface

Informs the server that the client will not be using this
	protocol object anymore. This does not affect any other
	objects, wl_subsurface objects included.
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-GET_SUBSURFACE ((OBJ WL_SUBCOMPOSITOR) CLIENT ID SURFACE PARENT)
  ";; give a surface the role sub-surface

Create a sub-surface interface for the given surface, and
	associate it with the given parent surface. This turns a
	plain wl_surface into a sub-surface.

	The to-be sub-surface must not already have another role, and it
	must not have an existing wl_subsurface object. Otherwise the
	bad_surface protocol error is raised.

	Adding sub-surfaces to a parent is a double-buffered operation on the
	parent (see wl_surface.commit). The effect of adding a sub-surface
	becomes visible on the next time the state of the parent surface is
	applied.

	The parent surface must not be one of the child surface's descendants,
	and the parent must be different from the child surface, otherwise the
	bad_parent protocol error is raised.

	This request modifies the behaviour of wl_surface.commit request on
	the sub-surface, see the documentation on wl_subsurface interface.

Arguments:
id::new_id: the new sub-surface object ID
surface::object: the surface to be turned into a sub-surface
parent::object: the parent surface
"
  (ERROR "Unimplemented"))

(DEFMETHOD ENUM-ERROR ((OBJ WL_SUBCOMPOSITOR) VALUE)
  ";; "
  (CASE VALUE (0 'BAD_SURFACE) (1 'BAD_PARENT)))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_SUBCOMPOSITOR) EVENT) (CASE EVENT))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_SUBCOMPOSITOR) OPCODE)
  (NTH OPCODE '(REQ-DESTROY REQ-GET_SUBSURFACE)))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_SUBCOMPOSITOR) OPCODE)
  (NTH OPCODE '(NIL (NEW_ID OBJECT OBJECT))))

(DEFPACKAGE :WL/WL_SUBSURFACE
  (:USE :CL :WL)
  (:EXPORT WL_SUBSURFACE
           REQ-DESTROY
           REQ-SET_POSITION
           REQ-PLACE_ABOVE
           REQ-PLACE_BELOW
           REQ-SET_SYNC
           REQ-SET_DESYNC))

(IN-PACKAGE :WL/WL_SUBSURFACE)

(DEFCLASS WL_SUBSURFACE (WL-OBJECT)
          ((CLIENT :INITARG :CLIENT :ACCESSOR CLIENT))
          (:DOCUMENTATION "sub-surface interface to a wl_surface

An additional interface to a wl_surface object, which has been
      made a sub-surface. A sub-surface has one parent surface. A
      sub-surface's size and position are not limited to that of the parent.
      Particularly, a sub-surface is not automatically clipped to its
      parent's area.

      A sub-surface becomes mapped, when a non-NULL wl_buffer is applied
      and the parent surface is mapped. The order of which one happens
      first is irrelevant. A sub-surface is hidden if the parent becomes
      hidden, or if a NULL wl_buffer is applied. These rules apply
      recursively through the tree of surfaces.

      The behaviour of a wl_surface.commit request on a sub-surface
      depends on the sub-surface's mode. The possible modes are
      synchronized and desynchronized, see methods
      wl_subsurface.set_sync and wl_subsurface.set_desync. Synchronized
      mode caches the wl_surface state to be applied when the parent's
      state gets applied, and desynchronized mode applies the pending
      wl_surface state directly. A sub-surface is initially in the
      synchronized mode.

      Sub-surfaces also have another kind of state, which is managed by
      wl_subsurface requests, as opposed to wl_surface requests. This
      state includes the sub-surface position relative to the parent
      surface (wl_subsurface.set_position), and the stacking order of
      the parent and its sub-surfaces (wl_subsurface.place_above and
      .place_below). This state is applied when the parent surface's
      wl_surface state is applied, regardless of the sub-surface's mode.
      As the exception, set_sync and set_desync are effective immediately.

      The main surface can be thought to be always in desynchronized mode,
      since it does not have a parent in the sub-surfaces sense.

      Even if a sub-surface is in desynchronized mode, it will behave as
      in synchronized mode, if its parent surface behaves as in
      synchronized mode. This rule is applied recursively throughout the
      tree of surfaces. This means, that one can set a sub-surface into
      synchronized mode, and then assume that all its child and grand-child
      sub-surfaces are synchronized, too, without explicitly setting them.

      Destroying a sub-surface takes effect immediately. If you need to
      synchronize the removal of a sub-surface to the parent surface update,
      unmap the sub-surface first by attaching a NULL wl_buffer, update parent,
      and then destroy the sub-surface.

      If the parent wl_surface object is destroyed, the sub-surface is
      unmapped.
"))

(DEFMETHOD REQ-DESTROY ((OBJ WL_SUBSURFACE) CLIENT)
  ";; remove sub-surface interface

The sub-surface interface is removed from the wl_surface object
	that was turned into a sub-surface with a
	wl_subcompositor.get_subsurface request. The wl_surface's association
	to the parent is deleted. The wl_surface is unmapped immediately.
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-SET_POSITION ((OBJ WL_SUBSURFACE) CLIENT X Y)
  ";; reposition the sub-surface

This schedules a sub-surface position change.
	The sub-surface will be moved so that its origin (top left
	corner pixel) will be at the location x, y of the parent surface
	coordinate system. The coordinates are not restricted to the parent
	surface area. Negative values are allowed.

	The scheduled coordinates will take effect whenever the state of the
	parent surface is applied. When this happens depends on whether the
	parent surface is in synchronized mode or not. See
	wl_subsurface.set_sync and wl_subsurface.set_desync for details.

	If more than one set_position request is invoked by the client before
	the commit of the parent surface, the position of a new request always
	replaces the scheduled position from any previous request.

	The initial position is 0, 0.

Arguments:
x::int: x coordinate in the parent surface
y::int: y coordinate in the parent surface
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-PLACE_ABOVE ((OBJ WL_SUBSURFACE) CLIENT SIBLING)
  ";; restack the sub-surface

This sub-surface is taken from the stack, and put back just
	above the reference surface, changing the z-order of the sub-surfaces.
	The reference surface must be one of the sibling surfaces, or the
	parent surface. Using any other surface, including this sub-surface,
	will cause a protocol error.

	The z-order is double-buffered. Requests are handled in order and
	applied immediately to a pending state. The final pending state is
	copied to the active state the next time the state of the parent
	surface is applied. When this happens depends on whether the parent
	surface is in synchronized mode or not. See wl_subsurface.set_sync and
	wl_subsurface.set_desync for details.

	A new sub-surface is initially added as the top-most in the stack
	of its siblings and parent.

Arguments:
sibling::object: the reference surface
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-PLACE_BELOW ((OBJ WL_SUBSURFACE) CLIENT SIBLING)
  ";; restack the sub-surface

The sub-surface is placed just below the reference surface.
	See wl_subsurface.place_above.

Arguments:
sibling::object: the reference surface
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-SET_SYNC ((OBJ WL_SUBSURFACE) CLIENT)
  ";; set sub-surface to synchronized mode

Change the commit behaviour of the sub-surface to synchronized
	mode, also described as the parent dependent mode.

	In synchronized mode, wl_surface.commit on a sub-surface will
	accumulate the committed state in a cache, but the state will
	not be applied and hence will not change the compositor output.
	The cached state is applied to the sub-surface immediately after
	the parent surface's state is applied. This ensures atomic
	updates of the parent and all its synchronized sub-surfaces.
	Applying the cached state will invalidate the cache, so further
	parent surface commits do not (re-)apply old state.

	See wl_subsurface for the recursive effect of this mode.
"
  (ERROR "Unimplemented"))

(DEFMETHOD REQ-SET_DESYNC ((OBJ WL_SUBSURFACE) CLIENT)
  ";; set sub-surface to desynchronized mode

Change the commit behaviour of the sub-surface to desynchronized
	mode, also described as independent or freely running mode.

	In desynchronized mode, wl_surface.commit on a sub-surface will
	apply the pending state directly, without caching, as happens
	normally with a wl_surface. Calling wl_surface.commit on the
	parent surface has no effect on the sub-surface's wl_surface
	state. This mode allows a sub-surface to be updated on its own.

	If cached state exists when wl_surface.commit is called in
	desynchronized mode, the pending state is added to the cached
	state, and applied as a whole. This invalidates the cache.

	Note: even if a sub-surface is set to desynchronized, a parent
	sub-surface may override it to behave as synchronized. For details,
	see wl_subsurface.

	If a surface's parent surface behaves as desynchronized, then
	the cached state is applied on set_desync.
"
  (ERROR "Unimplemented"))

(DEFMETHOD ENUM-ERROR ((OBJ WL_SUBSURFACE) VALUE)
  ";; "
  (CASE VALUE (0 'BAD_SURFACE)))

(DEFMETHOD MATCH-EVENT-OPCODE ((OBJ WL_SUBSURFACE) EVENT) (CASE EVENT))

(DEFMETHOD MATCH-REQUEST-OPCODE ((OBJ WL_SUBSURFACE) OPCODE)
  (NTH OPCODE
       '(REQ-DESTROY REQ-SET_POSITION REQ-PLACE_ABOVE REQ-PLACE_BELOW
         REQ-SET_SYNC REQ-SET_DESYNC)))

(DEFMETHOD GET-REQUEST-ARG-TYPES ((OBJ WL_SUBSURFACE) OPCODE)
  (NTH OPCODE '(NIL (INT INT) (OBJECT) (OBJECT) NIL NIL)))
