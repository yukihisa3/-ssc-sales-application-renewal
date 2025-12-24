# CAINZ

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/CAINZ.CL`

## ソースコード

```jcl
/.**************************************************************./
/.*      NAME･････CAINZ                                        *./
/.*      MENU･････                                             *./
/.*                                                            *./
/.**************************************************************./

    PGM

    SNDMSG MSG-'ｶｲﾝｽﾞﾎｰﾑ発注集計表',TO-XCTL

    CALL PSY0005B.TOKCLIBO

    RETURN

```
