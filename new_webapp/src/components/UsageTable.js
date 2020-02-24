import React, { useState } from "react";
import { EditingState } from '@devexpress/dx-react-grid';
import {
  Grid,
  Table,
  TableHeaderRow,
  TableEditColumn,
  TableInlineCellEditing,
} from '@devexpress/dx-react-grid-bootstrap4';
import { connect } from "react-redux";
import { addFact } from "../actions";

const generateRows = (facts) => {
    const rows = facts.map((element) => {
      let row = [];
      for (let index = 0; index < element.usage.length; index++) {
        let argName = "arg" + index;
        row[argName] = element.usage[index];
      }
      row["id"] = element.id;
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

const mapDispatchToProps = (dispatch) => {
  return {
    keepUsage: (keptUsages) => {keptUsages.forEach(row => dispatch(addFact(row)))}
  }
};

const UsageTableBase = ({numColumns, rows, keepUsage}) => {
    let cols = [];
    for (let index = 0; index < numColumns; index++) {
      cols = cols.concat({name: "arg" + index, title: "arg" + index});
    }
    const [columns] = useState(cols);

    const commitChanges = ({ added, changed, deleted }) => {
      let changedRows;
      if (added) {
        debugger;
      }
      if (changed) {
        debugger;
      }
      if (deleted) {
        const deletedSet = new Set(deleted);
        const keptRows = rows.filter(row => deletedSet.has(row.id));
        keepUsage(keptRows);
      }
    };

    return (<div>
        <Grid
            rows={generateRows(rows)}
            columns={columns}
            getRowId={row => row.id}
        >
          <EditingState
            onCommitChanges={commitChanges}
            addedRows={[]}
          />
            <Table/>
            <TableHeaderRow/>
            <TableEditColumn
              showDeleteCommand
              messages={{
                deleteCommand: "Keep usage"
              }}
            />
        </Grid>
    </div>);
};

const UsageTable = connect(null, mapDispatchToProps)(UsageTableBase);

export default UsageTable;