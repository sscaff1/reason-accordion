open Webapi.Dom;

external asDomElement : 'a => Dom.element = "%identity";

let unwrapElement =
  fun
  | Some(v) => v
  | None => raise(Invalid_argument("Passed none to unwrap"));

type action =
  | Expand(Dom.element, string)
  | Collapse(Dom.element, string);

type accordionItem = {
  header: Dom.element,
  content: Dom.element,
  headerHeight: int,
  contentHeight: int,
  el: Dom.element
};

let getClassList = item => item |> Element.classList;

let toggleClasses = (newClass, oldClass, classList) => {
  DomTokenList.add(newClass, classList);
  DomTokenList.remove(oldClass, classList);
};

let toggle = toggleAction =>
  switch toggleAction {
  | Expand(item, height) =>
    let maxHeight = "max-height:" ++ height ++ "px";
    item |> Element.setAttribute("style", maxHeight);
    item |> getClassList |> toggleClasses("expanded", "collapsed");
  | Collapse(item, height) =>
    let maxHeight = "max-height:" ++ height ++ "px";
    item |> Element.setAttribute("style", maxHeight);
    item |> getClassList |> toggleClasses("collapsed", "expanded");
  };

let accordion =
  document |> Document.querySelector(".accordion") |> unwrapElement;

let sections = accordion |> Element.querySelectorAll(".section");

let getSectionElement = (withClass, section) =>
  section |> Element.querySelector(withClass) |> unwrapElement;

let getHeight = element => element |> Element.clientHeight;

let handleHeaderClick = (e, headerHeight, contentHeight) => {
  let element = MouseEvent.target(e) |> asDomElement;
  let section = element |> Element.closest(".section");
  section |> getClassList |> DomTokenList.contains("collapsed") ?
    toggle(Expand(section, string_of_int(headerHeight + contentHeight))) :
    toggle(Collapse(section, string_of_int(headerHeight)));
};

let composeItem = node => {
  let section = node |> asDomElement;
  let header = section |> getSectionElement(".header");
  let content = section |> getSectionElement(".content");
  let headerHeight = header |> getHeight;
  let contentHeight = content |> getHeight;
  let intialHeight =
    section |> getClassList |> DomTokenList.contains("collapsed") ?
      headerHeight : contentHeight;
  section
  |> Element.setAttribute(
       "style",
       "max-height:" ++ string_of_int(intialHeight) ++ "px"
     );
  header
  |> Element.addClickEventListener(e =>
       handleHeaderClick(e, headerHeight, contentHeight)
     );
  {
    header,
    content,
    headerHeight: header |> getHeight,
    contentHeight: content |> getHeight,
    el: node |> Node.parentElement |> unwrapElement
  };
};

let accordionElements = NodeList.toArray(sections) |> Array.map(composeItem);