import React from 'react';
import _ from "underscore";
import { connect } from 'react-redux';
import { setExamples, setExampleEditingRow, setExampleEditingCol, increaseArgs, 
    decreaseArgs, setArgNames, setSearchType } from '../actions';
import { exampleToNamedArgs, replaceIthArgName, parseResultToStr, argNamesOf } from '../utilities/args';
import { Button, Table } from 'react-bootstrap';
import { EditableRow } from './EditableTable/EditableRow';
import { v4 } from 'uuid';
// import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
// import { faPlusSquare } from '@fortawesome/free-regular-svg-icons';
// import { faEraser } from '@fortawesome/free-solid-svg-icons';

const mapStateToProps = (state) => {
    return {
        argNames: state.spec.argNames,
        exampleRows: state.spec.rows,
        parsedType: state.spec.parsedType,
        editingRowId: state.spec.editingExampleRow,
        editingColId: state.spec.editingExampleCol,
    }
};

const mapDispatchToProps = (dispatch) => {
    return {
        setFacts: (changedFacts) => dispatch(setExamples(changedFacts)),
        setSearchType: (query) => dispatch(setSearchType(query)),
        setEditingRowId: (editingCellId) => dispatch(setExampleEditingRow(editingCellId)),
        setEditingColId: (editingColId) => dispatch(setExampleEditingCol(editingColId)),
        increaseArgs: () => dispatch(increaseArgs()),
        decreaseArgs: () => dispatch(decreaseArgs()),
        setArgNames: (newArgNames) => dispatch(setArgNames(newArgNames)),
        clearExamples: _ => dispatch(setExamples([])),
    }
};

const ExampleTableBase = ({
    argNames, exampleRows, editingRowId, editingColId, parsedType,
    setFacts, setEditingRowId, setEditingColId, setSearchType,
    increaseArgs, decreaseArgs, setArgNames, clearExamples}) => {

    const rows = exampleToNamedArgs(exampleRows);
    const colNames = [...argNames, "output"];
    const columns = colNames.map(name => { return { name: name, title: name } });
    const argRows = exampleToNamedArgs([{
        inputs: argNames,
        output: "output",
        id: "argRow",
        isLoading: false,
    }]);
    const argRow = argRows[0];

    const commitChanges = ({ added, changed, deleted }) => {
        let changedRows;
        if (added) {
            const startingAddedId = rows.length > 0
                ? Math.max(rows[rows.length - 1].id, rows[0].id) + 1
                : 0;
            changedRows = [
                ...rows,
                ...added.map((row, index) => ({
                    id: startingAddedId + index,
                    ...row,
                })),
            ];
            setEditingRowId(startingAddedId);
            setEditingColId(0);
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

    const onUpdateCell = ({ colName, rowName }) => e => {
        const newValue = e.target.value;
        const oldRow = rows.filter(row => row.id === rowName);
        const newRow = { ...oldRow[0], [colName]: newValue };
        commitChanges({ changed: { [rowName]: newRow } });
    };

    const onChangeArgName = ({ colName }) => e => {
        const newArgName = e.target.value;
        const newArgs = [
            ...argNames.slice(0, colName),
            newArgName,
            ...argNames.slice(colName+1)
        ];
        setArgNames(newArgs);

        // if the user didn't change the argument number, 
        // we update the query according to users' changes in the argument names
        if(parsedType && argNamesOf(parsedType).length === newArgs.length) {
            const newParseResult = replaceIthArgName(parsedType, parseInt(colName), newArgName);
            // if the arg name is updated, otherwise do not update the query
            if(!_.isEqual(newParseResult, parsedType)) {
                console.log("two parse results are different, set a new search type");
                const newQuery = parseResultToStr(newParseResult);
                setSearchType({query: newQuery});
            }
        }
    }

    const createNewExample = () => {
        const newRowId = v4();
        const changedRows = [
            ...rows,
            {
                id: newRowId,
                inputs: _.times(argNames.length, () => undefined),
                output: undefined,
            },
        ];
        setEditingRowId(newRowId);
        setEditingColId(0);
        setFacts(changedRows);
    };

    const hasNewExample = _.any(exampleRows, ({ inputs, output }) => {
        return _.all([output, ...inputs], x => (_.isNull(x) || _.isUndefined(x)));
    });

    const hasAnExample = !_.isEmpty(exampleRows);

    return (
        <div className="">
            <div>
                <div className="row mb-3">
                    <div className="col text-left">
                        <Button
                            variant="primary"
                            size="sm"
                            onClick={() => createNewExample()}
                            disabled={hasNewExample}
                            >
                            {/* <FontAwesomeIcon icon={ faPlusSquare } /> */}
                            Add Example
                        </Button>{' '}
                        <Button
                            variant="primary"
                            size="sm"
                            onClick={clearExamples}
                            disabled={!hasAnExample}
                            >
                            {/* <FontAwesomeIcon icon={ faEraser } /> */}
                            Clear Examples
                        </Button>{' '}
                    </div>
                </div>
                <div>
                    <Table className="table">
                        <thead>
                            <EditableRow
                                row={argRow}
                                columns={columns}
                                editingRowId={editingRowId}
                                editingColId={editingColId}
                                onClickEdit={({rowName, colName}) => {
                                    setEditingRowId(rowName);
                                    setEditingColId(colName);
                                    }
                                }
                                onClickSave={() => {
                                    setEditingRowId(null);
                                    setEditingColId(null);
                                    }
                                }
                                onUpdateCell={onChangeArgName}
                                increaseArgs={increaseArgs}
                                decreaseArgs={decreaseArgs}
                                key={argRow.id} 
                                />
                        </thead>
                        <tbody>
                            {rows.map((row) => {
                                return (<EditableRow
                                    row={row}
                                    columns={columns}
                                    editingRowId={editingRowId}
                                    editingColId={editingColId}
                                    onClickEdit={({rowName, colName}) => {
                                        setEditingRowId(rowName);
                                        setEditingColId(colName);
                                        }
                                    }
                                    onClickSave={() => {
                                        setEditingRowId(null);
                                        setEditingColId(null);
                                        }
                                    }
                                    onClickRemove={(rowId) => commitChanges({ deleted: [rowId] })}
                                    onUpdateCell={onUpdateCell}
                                    key={row.id} 
                                    />);
                            })}
                            
                        </tbody>
                    </Table>
                </div>
            </div>
        </div>
    );
};

const ExampleTable = connect(mapStateToProps, mapDispatchToProps)(ExampleTableBase);

export default ExampleTable;