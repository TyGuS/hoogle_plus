import React, { useState } from "react";
import { EditingState } from '@devexpress/dx-react-grid';
import {
  Grid,
  Table,
  TableHeaderRow,
  TableEditColumn,
  TableEditRow,
  TableInlineCellEditing,
} from '@devexpress/dx-react-grid-bootstrap4';
import { connect } from "react-redux";
import { addFact, updateCandidateUsage } from "../actions";

const getArgNames = () => {return ["arg0", "arg1"];}

const generateRows = (facts) => {
    if (!facts) {
      return [];
    }
    const rows = facts.map((element) => {
      let row = [];
      for (let index = 0; index < element.usage.length - 1; index++) {
        let argName = "arg" + index;
        row[argName] = element.usage[index];
      }
      row["result"] = element.usage[element.usage.length - 1];
      row["id"] = element.id;
      return row;
    });
    return rows;
    // return [
    //   {
    //     id: 0,
    //     arg0: "foo",
    //     arg1: "bar",
    //     result: "bax"
    //   }]
  }

const mapDispatchToProps = (dispatch) => {
  return {
    keepUsage: (keptUsages) => {keptUsages.forEach(row => dispatch(addFact(row)))},
    updateUsage: (updatedRow) => {updateCandidateUsage(updatedRow)(dispatch)}
  }
};

const UsageTableBase = ({
  candidateId, code,
  numColumns, rows:stateRows,
  keepUsage, updateUsage}) => {
    const internalRows = generateRows(stateRows);
    const argNames = getArgNames();
    let cols = [];
    for (let index = 0; index < numColumns - 1; index++) {
      cols = cols.concat({name: "arg" + index, title: "arg" + index});
    }
    cols = cols.concat({name: "result", title: "result"})
    const [columns] = useState(cols);

    const commitChanges = ({ added, changed, deleted }) => {
      let changedRows;
      if (added) {
        const addedSet = new Set(added);
        const addedRows = internalRows.filter(row => addedSet.has(row.id));
        debugger;
      }
      if (changed) {
        const changedIds = new Set(Object.keys(changed));
        const modifiedRows = internalRows
          .filter(row => changedIds.has(row.id))
          .map(row => ({ ...row, ...changed[row.id]}));
        // changedRows = internalRows.map(row => (changed[row.id] ? { ...row, ...changed[row.id] } : row));
        modifiedRows.forEach(modifiedRow => {
          const args = argNames.map(name => modifiedRow[name]);
          updateUsage({
            candidateId, code, args,
            usageId: modifiedRow.id,
          })
        });
      }
      if (deleted) {
        const deletedSet = new Set(deleted);
        const keptRows = internalRows.filter(row => deletedSet.has(row.id));
        keepUsage(keptRows);
      }
    };

    return (<div>
        <Grid
            rows={internalRows}
            columns={columns}
            getRowId={row => row.id}
        >
          <EditingState
            onCommitChanges={commitChanges}
            addedRows={[]}
            columnExtensions={[{columnName: "result", editingEnabled:false}]}
          />
          <Table/>
          <TableHeaderRow/>
          <TableEditRow/>
          <TableEditColumn
            showDeleteCommand
            showEditCommand
            messages={{
              deleteCommand: "Keep usage"
            }}
          />
        </Grid>
    </div>);
};

const UsageTable = connect(null, mapDispatchToProps)(UsageTableBase);

export default UsageTable;