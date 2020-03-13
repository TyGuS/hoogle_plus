import React, { useState } from 'react';
import _ from "underscore";
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
import { getArgNames, usageToNamedArgs } from '../utilities/args';
import { Button, ButtonGroup } from 'react-bootstrap';
import { SpinnableCell } from './SpinnableCell';
import { v4 } from "uuid";


const mapStateToProps = (state) => {
  return {
    numArgs: state.spec.numArgs,
    rows: usageToNamedArgs(state.spec.rows),
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

const ExampleTableBase = ({
  numArgs, rows, editingRowId,
  setFacts, setEditingRowId, increaseArgs, decreaseArgs}) => {

    const argNames = getArgNames(numArgs);
    const colNames = [...argNames, "result"];
    const columns = colNames.map(name => {return {name: name, title: name}});
    const [addedRows, setAddedRows] = useState([]);

    const changeAddedRows = (value) => {
        setAddedRows(value);
    }

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

    const addEmptyRow = () => {
      const newId = v4();
      return commitChanges({added: usageToNamedArgs([{
        id: newId,
        usage: _.times(numArgs, () => ""),
      }]) })
    };

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
                addedRows={addedRows}
                onAddedRowsChange={changeAddedRows}
                onCommitChanges={commitChanges}
              />
              <Table
                cellComponent={SpinnableCell}
              />
              <TableHeaderRow
                contentComponent={(rest) =>
                  <TableHeaderRow.Content {...rest} align="center"/>
                  }
              />
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