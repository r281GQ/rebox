[@react.component]
let make = () =>
  <div>
    {ReasonReact.string("message")}
    <Box p={`sc(2)} mt={`sc(1)}> {ReasonReact.string("message")} </Box>
  </div>;