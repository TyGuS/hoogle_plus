import React from 'react';

import { Button, OverlayTrigger, Tooltip } from 'react-bootstrap';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faTrashAlt, faPlusCircle, faMinusCircle } from '@fortawesome/free-solid-svg-icons';

import { EditableCell } from './EditableCell';

/**
 * Editable Row:
 * Props:
 *   - columns : [{name, title}]
 *   - row : {id, ...colName}
 *   - editingRowId : str
 *   - onUpdateCell : ({value, colName}, row) -> event -> ()
 *   - onClickEdit : (row) -> ()
 *   - onClickRemove : (id) -> ()
 *   - onClickSave : (row) -> ()
 */
export const EditableRow = ({row, columns, editingRowId, editingColId,
    onUpdateCell, onClickEdit, onClickSave, onClickRemove,
    increaseArgs, decreaseArgs}) => {
    const rowWithOrder = columns.map((col, idx) => {
      const colName = col.title;
      if (colName !== "output") {
        return {value:row[idx], colName: idx, rowId: row.id};
      } else {
        return {value:row[colName], colName, rowId: row.id};
      }
    });

    const onKeyPress = (event) => {
      if (event.key === "Enter") {
        onClickSave(row);
      }
    };

    const rowCells = rowWithOrder.map((cell, k) => {
        let colName = k;
        if (cell.colName === "output") {
          colName = cell.colName;
        }
        return (<EditableCell
            key={colName}
            type="text"
            cell={cell}
            value={cell.value}
            className="form-control"
            editingRowId={editingRowId}
            editingColId={editingColId}
            onKeyPress={onKeyPress}
            onUpdateCell={onUpdateCell}
            onClickEdit={onClickEdit}
            onClickSave={onClickSave}
            />);
    });

    if(row.id === "argRow") {
        return (
            <tr>
                {rowCells}
                <th className="row_controls">
                    
                    <OverlayTrigger
                        placement="top"
                        overlay={<Tooltip>Remove the last argument</Tooltip>}
                    >
                        <Button
                            variant="link"
                            onClick={decreaseArgs}
                            disabled={columns.length < 3}
                            style={columns.length < 3 ? { pointerEvents: 'none' } : {}}
                        >
                            <FontAwesomeIcon icon={ faMinusCircle } />
                        </Button>
                    </OverlayTrigger>
                    <OverlayTrigger
                        placement="top"
                        overlay={<Tooltip>Add another argument</Tooltip>}
                    >
                        <Button
                            variant="link"
                            onClick={increaseArgs}
                        >
                            <FontAwesomeIcon icon={ faPlusCircle } />
                        </Button>
                    </OverlayTrigger>
                </th>
            </tr>
        );
    } else {
        return (
            <tr>
                {rowCells}
                <td className="row_controls">
                    <Button
                        variant="link"
                        onClick={() => onClickRemove(row.id)}
                    >
                        <FontAwesomeIcon icon={ faTrashAlt } />
                    </Button>
                </td>
            </tr>
        );
    }

  };