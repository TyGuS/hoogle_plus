import React from 'react';

import { Button } from 'react-bootstrap';
import Highlight from 'react-highlight.js';
import OutsideClickHandler from 'react-outside-click-handler';


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
export const EditableRow = ({row, columns, editingRowId,
    onUpdateCell, onClickEdit, onClickSave, onClickRemove}) => {
    const rowWithOrder = columns.map(col => {return {value: row[col.name], colName:col.title}});
    const rowCells = rowWithOrder.map(({value:r}, k) => {
      return (
        <td key={k}>
          <Highlight language="haskell">
            {r}
          </Highlight>
        </td>)
    });

    const onBlur = (e) => {
      console.log("blur", e);
      // onClickSave(row);
    };

    const onFocus = (e) => {
      console.log("focus", e);
      // onClickEdit(row);
    };

    const onKeyPress = (event) => {
      if (event.key === "Enter") {
        onClickSave(row);
      }
    };

    if (row.id === editingRowId) {
      const editableRowCells = rowWithOrder.map((cell, k) => {
        return (<td key={cell.colName}>
          <input
            autoFocus={k === 0}
            key={cell.colName}
            type="text"
            className="form-control"
            onChange={onUpdateCell(cell, row)}
            value={cell.value}
            onBlur={onBlur}
            onFocus={onFocus}
            onKeyPress={onKeyPress}
          />
        </td>);
      });
      return (
        <tr>
          <td className="row_controls">
            <Button
              variant="link"
              onClick={() => onClickSave(row)}
            >
              Save
            </Button>
            <Button
              variant="link"
              onClick={() => onClickRemove(row.id)}
            >
              Cancel
            </Button>
          </td>
          {editableRowCells}
        </tr>
      );
    }

    return (
      <tr>
        <td className="row_controls">
          <Button
            variant="link"
            onClick={() => onClickEdit(row.id)}
          >
            Edit
          </Button>
          <Button
            variant="link"
            onClick={() => onClickRemove(row.id)}
          >
            Remove
          </Button>
        </td>
       {rowCells}
      </tr>
    );
  };