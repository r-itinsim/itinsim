# Metadata classes --------------------------------------------------------

iti_meta.arrivals <- "iti_arrivals"
iti_meta.resources <- "iti_resources"
iti_meta.attributes <- "iti_attributes"

#' Common metadata constants of IT infrastructure simulation framework
#' @export
iti_metadata <- gendatypes::c_extended(
  iti_meta.arrivals,
  iti_meta.resources,
  iti_meta.attributes,
  as.list = TRUE
)

# IT infrastructure classes -----------------------------------------------

iti_classes.infrastructure <- "IT infrastructure"
iti_classes.scheduler <- "Scheduler"
iti_classes.host <- "Host"
iti_classes.vm <- "VM"
iti_classes.server <- "Server"

#' Common constants for IT infrastructure entities
#' @export
iti_entities <- gendatypes::c_extended(
  iti_classes.infrastructure,
  iti_classes.scheduler,
  iti_classes.host,
  iti_classes.vm,
  iti_classes.server,
  as.list = TRUE
)

# Requests classes --------------------------------------------------------

request_classes.http <- "http"
request_classes.ftp <- "ftp"
request_classes.icmp <- "icmp"
request_classes.tcp_ip <- "tcp/ip"
request_classes.rpc <- "rpc"

#' Common constants for IT infrastructure entities
#' @export
iti_requests <- gendatypes::c_extended(
  request_classes.http,
  request_classes.ftp,
  request_classes.icmp,
  request_classes.tcp_ip,
  request_classes.rpc,
  as.list = TRUE
)
