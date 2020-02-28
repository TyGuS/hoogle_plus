import React from "react";
import { connect } from "react-redux";
import {Modal, Button} from "react-bootstrap";
import { setModalClosed } from "../actions";

const mapStateToProps = (state) => {
    return {
        isOpen: state.modal.isOpen,
    };
}

const mapDispatchToProps = (dispatch) => {
    return {
        onClose: () => dispatch(setModalClosed()),
    };
}

const TypeSelectionBase = (props) => {
    const {isOpen, onClose} = props;
    return (
        <>
        <Modal show={isOpen} onHide={onClose}>
        <Modal.Header closeButton>
          <Modal.Title>Modal heading</Modal.Title>
        </Modal.Header>
        <Modal.Body>Woohoo, you're reading this text in a modal!</Modal.Body>
        <Modal.Footer>
          <Button variant="secondary" onClick={onClose}>
            Close
          </Button>
          <Button variant="primary" onClick={onClose}>
            Save Changes
          </Button>
        </Modal.Footer>
      </Modal>
      </>
    );
}

export const TypeSelection = connect(mapStateToProps, mapDispatchToProps)(TypeSelectionBase);