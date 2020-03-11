import _ from "underscore";

// Ensure that if it's not specified, nothing is limited.
const defaultValue = !(process.env.REACT_APP_LIMIT_FEATURES || false);

const features = {
    search: {
        permitExamples: defaultValue,
        permitTypeCandidates: defaultValue,
    },
    results: {
        permitExamples: defaultValue,
        permitEditExamples: defaultValue,
        permitKeepUsage: defaultValue,
        enableGetMoreExamples: defaultValue,
    }
};

export const getDefaultFeatures = () => {
    return features;
}