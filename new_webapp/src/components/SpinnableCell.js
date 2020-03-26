import React from "react";
import { Table } from '@devexpress/dx-react-grid-bootstrap4';
import { BounceLoader } from "react-spinners";
import Highlight from "react-highlight.js";


export const SpinnableCell = ({ row, ...restProps }) => {
  const {value, ...notValueRest} = restProps;
  if (restProps.column.name === "output") {
    if (row.isLoading) {
      return (<Table.Cell row={row} {...restProps}>
        <BounceLoader loading={row.isLoading} />
      </Table.Cell>);
    }
    if (row.error) {
      return (<Table.Cell row={row} {...restProps}>
        <code className="error_message">
          {row.error}
        </code>
      </Table.Cell>);
    }
  }
  return (<Table.Cell row={row} align="center" {...notValueRest}>
    <Highlight language="haskell">{value}</Highlight>
    </Table.Cell>);
};
