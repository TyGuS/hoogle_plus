import React from 'react';
import _ from "underscore";
import { connect } from 'react-redux';
import { setExamples, setExampleEditingRow, increaseArgs, decreaseArgs } from '../actions';
import { getArgNames, exampleToNamedArgs } from '../utilities/args';
import { Button, ButtonGroup, Table } from 'react-bootstrap';
import { EditableRow } from './EditableTable/EditableRow';
import { v4 } from 'uuid';


const mapStateToProps = (state) => {
  return {
    numArgs: state.spec.numArgs,
    exampleRows: state.spec.rows,
    editingRowId: state.spec.editingExampleRow,
  }
};

const mapDispatchToProps = (dispatch) => {
  return {
    setFacts: (changedFacts) => dispatch(setExamples(changedFacts)),
    setEditingRowId: (editingCellId) => dispatch(setExampleEditingRow(editingCellId)),
    increaseArgs: () => dispatch(increaseArgs()),
    decreaseArgs: () => dispatch(decreaseArgs()),
  }
};

const ExampleTableBase = ({
  numArgs, exampleRows, editingRowId,
  setFacts, setEditingRowId, increaseArgs, decreaseArgs}) => {

    const rows = exampleToNamedArgs(exampleRows);
    const argNames = getArgNames(numArgs);
    const colNames = [...argNames, "output"];
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

    const onUpdateCell = ({colName}, row) => e => {
      const newValue = e.target.value;
      const newRow = {...row, [colName]:newValue};
      commitChanges({changed: {[row.id]: newRow}});
    };

    const createNewExample = () => {
      const newRowId = v4();
      const changedRows = [
        {
          id: newRowId,
          inputs: _.times(numArgs, () => undefined),
          output: undefined,
        },
        ...rows,
      ];
      setEditingRowId(newRowId);
      setFacts(changedRows);
    };

    const hasNewExample =  _.any(exampleRows, ({inputs, output}) => {
      return _.all([output, ...inputs], x => (_.isNull(x) || _.isUndefined(x)));
    });

    return (
      <div className="container">
        <div className="row">
          <div className="col-10">
            <Table>
              <thead>
                <tr>
                  <th className="row_controls"></th>
                  {columns.map((column, idx) => {return (<th key={idx}>{column.title}</th>)})}
                </tr>
              </thead>
              <tbody>
                {rows.map((row) => {
                  return (<EditableRow
                    row={row}
                    columns={columns}
                    editingRowId={editingRowId}
                    onClickEdit={() => setEditingRowId(row.id)}
                    onClickSave={() => setEditingRowId(null)}
                    onClickRemove={(rowId) => commitChanges({deleted: [rowId]})}
                    onUpdateCell={onUpdateCell}
                    key={row.id
                    }/>);
                })}
              </tbody>
            </Table>
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
            <Button
              className="mt-1"
              onClick={() => createNewExample()}
              disabled={hasNewExample}
            >
              Add Example
            </Button>
          </div>
        </div>
      </div>
    );
};

const ExampleTable = connect(mapStateToProps, mapDispatchToProps)(ExampleTableBase);

export default ExampleTable;