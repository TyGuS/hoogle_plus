import React, { useState } from "react";
import {
    Grid,
    Table,
    TableHeaderRow,
    TableEditColumn,
  } from '@devexpress/dx-react-grid-bootstrap4';

const generateRows = (facts) => {
    const rows = facts.map((element, idx) => {
      let row = [];
      for (let index = 0; index < element.length; index++) {
        let argName = "arg" + index;
        row[argName] = element[index];
      }
      row["id"] = idx;
      return row;
    });
    return rows;
    // return [
    //   {
    //     id: 0,
    //     arg0: "foo",
    //     arg1: "bar",
    //     arg2: "bax"
    //   }]
  }

const UsageTable = ({numColumns, rows}) => {
    let cols = [];
    for (let index = 0; index < numColumns; index++) {
      cols = cols.concat({name: "arg" + index, title: "arg" + index});
    }
    const [columns] = useState(cols);

    return (<div>
        <Grid
            rows={generateRows(rows)}
            columns={columns}
            getRowId={row => row.id}
        >
            <Table/>
            <TableHeaderRow/>
        </Grid>
    </div>);
};

export default UsageTable;