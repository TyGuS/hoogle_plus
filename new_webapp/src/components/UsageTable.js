import React, { useState } from "react";
import _ from "underscore";
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
import { addExample, updateCandidateUsages, addCandidateUsage } from "../actions";
import { getArgNames, exampleToNamedArgs, namedArgsToExample } from "../utilities/args";
import { SpinnableCell } from "./SpinnableCell";
import { getDefaultFeatures } from "../utilities/featureManager";
import { v4 } from "uuid";

const {results: resultsFeatures} = getDefaultFeatures();

const generateRows = (facts) => {
    if (!facts || facts.length < 1) {
      return [];
    }
    return exampleToNamedArgs(facts);
    // return [
    //   {
    //     id: 0,
    //     arg0: "foo",
    //     arg1: "bar",
    //     output: "bax"
    //   }]
  }

const mapDispatchToProps = (dispatch) => {
  return {
    keepUsage: (example) => {example.forEach(ex => {
        dispatch(addExample(ex));
    })},
    updateUsage: (updatedRow) => {dispatch(updateCandidateUsages(updatedRow))},
    addCandidateUsage: ({inputs, candidateId, code}) => dispatch(addCandidateUsage({inputs, candidateId, code}))
  }
};

const UsageTableBase = ({
  candidateId, qualCode, examplesShown, argNames,
  numColumns, rows:stateRows,
  keepUsage, updateUsage, addCandidateUsage}) => {

    const internalRows = generateRows(_.take(stateRows, examplesShown));
    // const argNames = getArgNames(numColumns - 1);

    let cols = [];
    let colExtensions = [];
    const width = Math.floor(1 / (numColumns + 1) * 100) + "%";
    for (let index = 0; index < numColumns - 1; index++) {
      cols = cols.concat({name: index, title: argNames[index]});
      colExtensions = colExtensions.concat({
        columnName: argNames[index],
        width: width,
      })
    };
    cols = cols.concat({name: "output", title: "output"});
    colExtensions = colExtensions.concat({
      columnName: "output",
      width: width,
    });

    const [newRow, updateNewRow] = useState([]);

    const commitChanges = ({ added, changed, deleted }) => {
      if (added) {
        const examples = namedArgsToExample(added, numColumns - 1);
        addCandidateUsage({inputs: examples[0].inputs, candidateId, code: qualCode});
      }
      if (changed) {
        const changedIds = new Set(Object.keys(changed));
        const modifiedRows = internalRows
          .filter(row => changedIds.has(row.id))
          .map(row => ({ ...row, ...changed[row.id]}));
        // changedRows = internalRows.map(row => (changed[row.id] ? { ...row, ...changed[row.id] } : row));
        modifiedRows.forEach(modifiedRow => {
          const inputs = argNames.map((name, idx) => modifiedRow[idx]);
          updateUsage({
            candidateId, code: qualCode, inputs,
            usageId: modifiedRow.id,
          })
        });
      }
      if (deleted) {
        const deletedSet = new Set(deleted);
        const keptRows = internalRows.filter(row => deletedSet.has(row.id));
        const keptExamples = namedArgsToExample(keptRows, numColumns - 1);
        keepUsage(keptExamples);
      }
    };

    const addedRowsChange = (value) => {
      if (! newRow.id) {
        console.log("added new row id");
        value.id = v4();
      }
      console.log("added rows change", value);
      updateNewRow(value);
    }

    return (<div>
        <Grid
            rows={internalRows}
            columns={cols}
            getRowId={row => row.id}
        >
          <EditingState
            onAddedRowsChange={addedRowsChange}
            onCommitChanges={commitChanges}
            addedRows={newRow}
            columnExtensions={[{columnName: "output", editingEnabled:false}]}
          />
          <Table cellComponent={SpinnableCell}
              columnExtensions={colExtensions}
          />
          <TableHeaderRow
            contentComponent={(rest) =>
              <TableHeaderRow.Content {...rest} align="center"/>
              }
          />
          <TableEditRow/>
          <TableEditColumn
            showAddCommand
            showDeleteCommand={resultsFeatures.permitKeepUsage}
            showEditCommand={resultsFeatures.permitEditExamples}
            messages={{
              deleteCommand: "Keep example",
              addCommand: "New example",
            }}
          />
        </Grid>
    </div>);
};

const UsageTable = connect(null, mapDispatchToProps)(UsageTableBase);

export default UsageTable;