import React, { useState } from 'react';
import { EditingState } from '@devexpress/dx-react-grid';
import {
  Grid,
  Table,
  TableHeaderRow,
  TableEditColumn,
  TableEditRow,
} from '@devexpress/dx-react-grid-bootstrap4';
import { connect } from 'react-redux';
import { setExamples, setExampleEditingRow, increaseArgs, decreaseArgs } from '../actions';
import { getArgNames } from '../utilities/args';
import { Button, ButtonGroup } from 'react-bootstrap';
import { SpinnableCell } from './SpinnableCell';
import { v4 as uuidv4 } from "uuid";


const mapStateToProps = (state) => {
  return {
    numArgs: state.spec.numArgs,
    rows: generateRows(state.spec.rows),
    editingRowId: state.spec.editingExampleRow,
  }
};

const mapDispatchToProps = (dispatch) => {
  return {
    setFacts: (changedFacts) => dispatch(setExamples(changedFacts)),
    setEditingRowId: (editingCells) => dispatch(setExampleEditingRow(editingCells)),
    increaseArgs: () => dispatch(increaseArgs()),
    decreaseArgs: () => dispatch(decreaseArgs()),
  }
};

// facts :: [{i/o}] -> [{}]
const generateRows = (facts) => {
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
  //     arg2: "bax"
  //   }]
}

const ExampleTableBase = ({
  numArgs, rows, editingRowId,
  setFacts, setEditingRowId, increaseArgs, decreaseArgs}) => {

    const argNames = getArgNames(numArgs);
    const colNames = [...argNames, "result"];
    const columns = colNames.map(name => {return {name: name, title: name}});

    const commitChanges = ({ added, changed, deleted }) => {
      let changedRows;
      if (added) {
        const startingAddedId = rows.length > 0
          ? Math.max(rows[rows.length - 1].id, rows[0].id) + 1
          : 0;
        changedRows = [
          ...added.map((row, index) => ({
            id: startingAddedId + index,
            ...row,
          })),
          ...rows,
        ];
        setEditingRowId(startingAddedId);
      }
      if (changed) {
        changedRows = rows.map(row => (changed[row.id] ? { ...row, ...changed[row.id] } : row));
      }
      if (deleted) {
        const deletedSet = new Set(deleted);
        changedRows = rows.filter(row => !deletedSet.has(row.id));
      }
      setFacts(changedRows);
    };

    const addEmptyRow = () => commitChanges({ added: [{id: uuidv4()}] });

    return (
      <div className="container">
        <div className="row">
          <div className="col-10">
            <Grid
              rows={rows}
              columns={columns}
              getRowId={row => row.id}
            >
              <EditingState
                editingRowId={editingRowId}
                onEditingCellsChange={setEditingRowId}
                addedRows={[]}
                onAddedRowsChange={addEmptyRow}
                onCommitChanges={commitChanges}
              />
              <Table
                cellComponent={SpinnableCell}
                />
              <TableHeaderRow />
              <TableEditRow/>
              <TableEditColumn
                showAddCommand
                showEditCommand
                showDeleteCommand
              />
            </Grid>
          </div>
          <div className="col-2">
            # Arguments
            <ButtonGroup>
              <Button
                onClick={decreaseArgs}
                disabled={numArgs < 2}>
                  -
              </Button>
              <Button onClick={increaseArgs}>
                +
              </Button>
            </ButtonGroup>
          </div>
        </div>
      </div>
    );
};

const ExampleTable = connect(mapStateToProps, mapDispatchToProps)(ExampleTableBase);

export default ExampleTable;