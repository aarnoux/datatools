import pandas as pd
from typing import Union


def describe(input: Union[dict, pd.DataFrame]) -> pd.DataFrame:
    """
    Returns a DataFrame with the data types, the number of unique and nan values of each column in the input DataFrame.
    Args:
        df (pd.DataFrame): The DataFrame to be described.
    Returns:
        pd.DataFrame: The DataFrame with the data types and unique values of each column.
    Examples:
        >>> df = pd.DataFrame({'col1': ['a', 'b', 'c'], 'col2': [1, 2, nan]})
        >>> describe_columns(df)
            Column Name Data Type Nb Unique Values Nb NA Values   Value Sample
        0       col1    object          3           0 (0%)          a
        1       col2     int64          2           1 (33%)         1
    """
    data_desc = []

    if isinstance(input, dict):
        input = pd.DataFrame(input)

    for c in input.columns:
        DATA_TYPE = input[c].dtype
        NB_UNIQUE = input[c].astype(str).nunique()
        NB_NAN = input[c].isna().sum()
        try:
            RANDOM = input[c].dropna().sample(n=1).values[0]
        except ValueError:  # if only NA values
            RANDOM = None

        data_desc.append(
            [
                c,
                DATA_TYPE,
                NB_UNIQUE,
                f"{NB_NAN} ({round(NB_NAN/input.shape[0]*100)}%)",
                RANDOM,
            ]
        )

    output = pd.DataFrame(
        data_desc,
        columns=[
            "column name",
            "data type",
            "nb unique values",
            "nb NA values",
            "value sample",
        ],
    )
    print(
        f"Number of rows: {input.shape[0]}\t",
        f"Number of columns: {input.shape[1]}",
    )

    return output
