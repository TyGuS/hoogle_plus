import React from "react";
import { connect } from "react-redux";
import {Modal, Button, ButtonGroup} from "react-bootstrap";
import { setModalClosed, selectType } from "../actions";

const mapStateToProps = (state) => {
    return {
        isOpen: state.modal.isOpen,
        typeOptions: state.spec.searchTypeOptions,
        exampleRows: state.spec.rows,
    };
}

const mapDispatchToProps = (dispatch) => {
    return {
        onClose: () => dispatch(setModalClosed()),
        selectType: ({typeOption, examples}) => dispatch(selectType({typeOption, examples})),
    };
}

const TypeSelectionBase = (props) => {
    const {onClose, selectType} = props;
    const {isOpen, typeOptions, exampleRows} = props;

    const mkSelection = (typeStr) => {
        const examples = exampleRows.map(row => row.usage);
        return selectType({
            typeOption: typeStr,
            examples,
        });
    }

    return (
        <>
        <Modal show={isOpen} onHide={onClose}>
        <Modal.Header closeButton>
          <Modal.Title>Which type looks right to you?</Modal.Title>
        </Modal.Header>
        <Modal.Body>
            <div>
                To help us give you the best results,
                help us narrow down the type signature.
                Please select one of the following:
            </div>
            <br/>
            <div className="container">
            <ButtonGroup vertical className="justify-content-center">
                {typeOptions.map((typeStr, idx) => {
                    return (<div className="row">
                        <Button
                            className="col-5"
                            onClick={() => mkSelection(typeStr)}>
                                {idx + 1}
                        </Button>
                        <span className="col-7">{typeStr}</span>
                    </div>);
                })}
            </ButtonGroup>
            </div>
        </Modal.Body>
        <Modal.Footer>
          <Button variant="secondary" onClick={onClose}>
            Close
          </Button>
        </Modal.Footer>
      </Modal>
      </>
    );
}

export const TypeSelection = connect(mapStateToProps, mapDispatchToProps)(TypeSelectionBase);