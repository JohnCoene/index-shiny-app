guide <- cicerone::Cicerone$
  new()$
  step(
  "legend",
  "Resilience Index",
  "The map shows resilience in Local Authorities. Resilience is a combination of vulnerability, capacity to cope, and exposure to shocks. 
    <span style = 'color:#3E2948; font-weight:bold;'>Areas coloured mauve</span> are highly vulnerable, with low capacity to cope."
)$
  step(
  el = ".sidebar-menu",
  title = "Choose theme",
  description = "Click the items here to view resilience to <strong>disasters and emergencies</strong>, <strong>health inequalities</strong>, or <strong>migration and displacement</strong>.",
  is_id = FALSE
)$
  step(
  ".treeview-menu",
  "Choose filters",
  "You can filter some types of resilience. For example, here you can filter Local Authorities with high risks of flooding or fires.",
  is_id = FALSE
)$
  step(
  ".lad-select",
  "Select Local Authorities",
  "Use this box to choose or search for Local Authorities. You can also click Local Authorities on the map to see more details, including their vulnerable neighbourhoods.",
  is_id = FALSE
)$
  step(
  "[data-value='Data']",
  "View the data",
  "Click this tab to see the underlying data rather than a map.",
  is_id = FALSE
)$
  step(
  "[data-value='Help']",
  "Get more information",
  "Click this tab for help and further information.",
  is_id = FALSE
)
