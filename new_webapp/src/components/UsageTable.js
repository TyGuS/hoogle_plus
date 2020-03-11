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
import { addFact, updateCandidateUsages } from "../actions";
import { getArgNames } from "../utilities/args";
import { SpinnableCell } from "./SpinnableCell";
import { getDefaultFeatures } from "../utilities/featureManager";

const {results: resultsFeatures} = getDefaultFeatures();

const generateRows = (facts) => {
    if (!facts || facts.length < 1) {
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
      return {...element, ...newFields};
    });
    return rows;
    // return [
    //   {
    //     id: 0,
    //     arg0: "foo",
    //     arg1: "bar",
    //     result: "bax"
    //     usage: [foo, var, bax]
    //   }]
  }

const mapDispatchToProps = (dispatch) => {
  return {
    keepUsage: (keptUsages) => {keptUsages.forEach(row => {
        const {id, usage} = row;
        dispatch(addFact({id, usage}));
    })},
    updateUsage: (updatedRow) => {dispatch(updateCandidateUsages(updatedRow))}
  }
};

const UsageTableBase = ({
  candidateId, code,
  numColumns, rows:stateRows,
  keepUsage, updateUsage}) => {
    const internalRows = generateRows(stateRows);
    const argNames = getArgNames(numColumns - 1);
    let cols = [];
    let colExtensions = [];
    const width = Math.floor(1 / (numColumns + 1) * 100) + "%";
    for (let index = 0; index < numColumns - 1; index++) {
      cols = cols.concat({name: "arg" + index, title: "arg" + index});
      colExtensions = colExtensions.concat({
        columnName: "arg" + index,
        width: width,
      })
    };
    cols = cols.concat({name: "result", title: "result"});
    colExtensions = colExtensions.concat({
      columnName: "result",
      width: width,
    });
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
          <Table cellComponent={SpinnableCell}
              columnExtensions={colExtensions}
          />
          <TableHeaderRow
            contentComponent={(rest) =>
              <TableHeaderRow.Content {...rest} align="left"/>
              }
          />
          <TableEditRow/>
          <TableEditColumn
            showDeleteCommand={resultsFeatures.permitKeepUsage}
            showEditCommand={resultsFeatures.permitEditExamples}
            messages={{
              deleteCommand: "Keep usage"
            }}
          />
        </Grid>
    </div>);
};

const UsageTable = connect(null, mapDispatchToProps)(UsageTableBase);

export default UsageTable;