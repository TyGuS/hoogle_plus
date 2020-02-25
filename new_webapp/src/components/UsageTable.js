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
import { BounceLoader } from "react-spinners";

const getArgNames = (numArgs) => {
  let argNames = [];
  for (let index = 0; index < numArgs; index++) {
    argNames = argNames.concat("arg" + index);
  }
  return argNames;
}

const SpinnableCell = ({row, ...restProps}) => {
  if (restProps.column.name === "result"){
    if (row.isLoading) {
      return (
        <Table.Cell row={row} {...restProps}>
          <BounceLoader loading={row.isLoading} />
        </Table.Cell>);
    }
    if (row.error) {
      return (
        <Table.Cell row={row} {...restProps}>
          <div className="error_message">
            {row.error}
          </div>
        </Table.Cell>
      );
    }
  }
  return (
    <Table.Cell row={row} {...restProps} />
  );
  }

const generateRows = (facts) => {
    if (!facts) {
      return [];
    }
    const argNames = getArgNames(facts[0].usage.length - 1);
    const rows = facts.map((element) => {
      let newFields = {};
      for (let index = 0; index < argNames.length; index++) {
        let argName = argNames[index];
        newFields[argName] = element.usage[index];
      }
      newFields["result"] = element.usage[element.usage.length - 1];
      const {usage, ...rest} = {...element, ...newFields};
      return rest;
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
    const argNames = getArgNames(numColumns - 1);
    let cols = [];
    for (let index = 0; index < numColumns - 1; index++) {
      cols = cols.concat({name: "arg" + index, title: "arg" + index});
    }
    cols = cols.concat({name: "result", title: "result"})
    const [columns] = useState(cols);

    const commitChanges = ({ added, changed, deleted }) => {
      if (added) {
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
          <Table cellComponent={SpinnableCell}/>
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