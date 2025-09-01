import pandas as pd


def describe_columns(df: pd.DataFrame) -> pd.DataFrame:
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

    for c in df.columns:
        DATA_TYPE = df[c].dtype
        NB_UNIQUE = df[c].astype(str).nunique()
        NB_NAN = df[c].isna().sum()
        try:
            RANDOM = df[c].dropna().sample(n=1).values[0]
        except ValueError:  # if only NA values
            RANDOM = None

        data_desc.append(
            [
                c,
                DATA_TYPE,
                NB_UNIQUE,
                f"{NB_NAN} ({round(NB_NAN/df.shape[0]*100)}%)",
                RANDOM,
            ]
        )

    df_output = pd.DataFrame(
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
        f"Number of rows: {df.shape[0]}\t",
        f"Number of columns: {df.shape[1]}",
    )

    return df_output
