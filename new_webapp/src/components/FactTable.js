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
import { setFacts, setEditingCells } from '../actions';

const getRowId = row => row.id;

const FocusableCell = ({ onClick, ...restProps }) => (
  <Table.Cell {...restProps} tabIndex={0} onFocus={onClick} />
);

const mapStateToProps = (state) => {
  return {
    numColumns: state.numArgs + 1,
    rows: generateRows(state.facts.rows),
    editingCells: state.facts.editingCells,
  }
};

const mapDispatchToProps = (dispatch) => {
  return {
    setFacts: (allFacts) => dispatch(setFacts(allFacts)),
    setEditingCells: (editingCells) => dispatch(setEditingCells(editingCells)),
  }
};

// facts :: [[i/o]] -> [{}]
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

const FactTableBase = ({numColumns, rows, setFacts, editingCells, setEditingCells}) => {
  let cols = [];
  for (let index = 0; index < numColumns; index++) {
    cols = cols.concat({name: "arg" + index, title: "arg" + index});
  }
  const [columns] = useState(cols);

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
    <div className="card">
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
  );
};

const FactTable = connect(mapStateToProps, mapDispatchToProps)(FactTableBase);

export default FactTable;