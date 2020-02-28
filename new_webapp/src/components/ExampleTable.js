import React, { useState } from 'react';
import { EditingState } from '@devexpress/dx-react-grid';
import {
  Grid,
  Table,
  TableHeaderRow,
  TableEditColumn,
  TableInlineCellEditing,
} from '@devexpress/dx-react-grid-bootstrap4';
import { connect } from 'react-redux';
import { setFacts, setEditingCells, increaseArgs, decreaseArgs } from '../actions';
import { getArgNames } from '../utilities/args';
import { Button } from 'react-bootstrap';

const getRowId = row => row.id;

const FocusableCell = ({ onClick, ...restProps }) => (
  <Table.Cell {...restProps} tabIndex={0} onFocus={onClick} />
);

const mapStateToProps = (state) => {
  return {
    numArgs: state.spec.numArgs,
    rows: generateRows(state.spec.rows),
    editingCells: state.spec.editingCells,
  }
};

const mapDispatchToProps = (dispatch) => {
  return {
    setFacts: (allFacts) => dispatch(setFacts(allFacts)),
    setEditingCells: (editingCells) => dispatch(setEditingCells(editingCells)),
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
  numArgs, rows, editingCells,
  setFacts, setEditingCells, increaseArgs, decreaseArgs}) => {

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
        setEditingCells([{ rowId: startingAddedId, columnName: columns[0].name }]);
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

    const addEmptyRow = () => commitChanges({ added: [{}] });

    return (
      <div className="container">
        <div className="row">
          <div className="col-10">
            <Grid
              rows={rows}
              columns={columns}
              getRowId={getRowId}
            >
              <EditingState
                onCommitChanges={commitChanges}
                editingCells={editingCells}
                onEditingCellsChange={setEditingCells}
                addedRows={[]}
                onAddedRowsChange={addEmptyRow}
              />
              <Table cellComponent={FocusableCell} />
              <TableHeaderRow />
              <TableInlineCellEditing selectTextOnEditStart />
              <TableEditColumn
                showAddCommand
                showDeleteCommand
              />
            </Grid>
          </div>
          <div className="col-2">
            <Button onClick={increaseArgs}>
              add argument
            </Button>
            <Button
              onClick={decreaseArgs}
              disabled={numArgs < 2}>
                remove argument
            </Button>
          </div>
        </div>
      </div>
    );
};

const ExampleTable = connect(mapStateToProps, mapDispatchToProps)(ExampleTableBase);

export default ExampleTable;