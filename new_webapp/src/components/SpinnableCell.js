import React from "react";
import { Table } from '@devexpress/dx-react-grid-bootstrap4';
import { BounceLoader } from "react-spinners";
import Highlight from "react-highlight.js";


export const SpinnableCell = ({ row, ...restProps }) => {
  const {value, ...notValueRest} = restProps;
  if (restProps.column.name === "result") {
    if (row.isLoading) {
      return (<Table.Cell row={row} {...restProps}>
        <BounceLoader loading={row.isLoading} />
      </Table.Cell>);
    }
    if (row.error) {
      return (<Table.Cell row={row} {...restProps}>
        <div className="error_message">
          {row.error}
        </div>
      </Table.Cell>);
    }
  }
  return (<Table.Cell row={row} align="left" {...notValueRest}>
    <Highlight language="haskell">{value}</Highlight>
    </Table.Cell>);
};
