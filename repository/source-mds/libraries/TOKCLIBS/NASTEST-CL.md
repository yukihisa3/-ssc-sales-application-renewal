# NASTEST

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/NASTEST.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *TEST                                                     *  ./
/. *     サカタのタネ　新基幹システム                        *  ./
/. *   SYSTEM-NAME :    在庫管理                             *  ./
/. *   JOB-ID      :    NASTEST(PZA01900)                    *  ./
/. *   JOB-NAME    :    年次在庫データ繰り越し               *  ./
/. *   UPDATE      :    2011/11/24 MIURA MOからLTOへ変更     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'NASTEST '
    VAR       ?STEP     ,STRING*8
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./

/.↓ NASﾗｲﾌﾞﾗﾘへのﾃﾞｰﾀ退避ﾃｽﾄ--------------------------./
    VAR       ?YOUBI    ,STRING*01,VALUE-'0'        /.曜日  　   ./
    VAR       ?FILEID   ,STRING*08                  /.退避ﾌｧｲﾙ   ./
    VAR       ?LIBID1   ,STRING*07,VALUE-'NASBLIB'  /.退避先LIB  ./
    VAR       ?LIBID    ,STRING*08,VALUE-'        ' /.退避先LIB  ./
    VAR       ?FILE     ,NAME!MOD!COM               /.退避ﾌｧｲﾙLIB./
    VAR       ?FILEN    ,NAME                       /.退避ﾌｧｲﾙ   ./
    VAR       ?LIBN     ,NAME                       /.退避先LIB  ./
    VAR       ?FILETXT  ,STRING*17
/.↑ NASﾗｲﾌﾞﾗﾘへのﾃﾞｰﾀ退避ﾃｽﾄ--------------------------./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?OPR1  :=  '　＃＃＃＃＃＃＃　年次更新処理　＃＃＃＃＃＃＃＃'
    ?OPR2  :=  ''
    ?OPR3  :=  '　　年次更新を行ないます。確認して下さい。'
    ?OPR4  :=  ''
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    DEFLIBL    TOKFLIB/TOKELIB

ZAIKOBAK:

    ?MSGX :=  '## 在庫ﾏｽﾀ ﾊﾞｯｸｱｯﾌﾟ中 ##'
    SNDMSG    ?MSGX,TO-XCTL

/.↓ NASﾗｲﾌﾞﾗﾘへのﾃﾞｰﾀ退避ﾃｽﾄ--------------------------./

/.  ↓ このSAVFILEをｺﾒﾝﾄにしNASﾗｲﾌﾞﾗﾘへの退避に変更----./
/.  SAVFILE FILE-ZAMZAIF.TOKFLIB/ZAMJISF.TOKFLIB,TODEV-LTO,
            ADD-@YES,MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END   ./

/.  ↓ ここからNASﾗｲﾌﾞﾗﾘへの退避処理-------------------./
ONCHECK: /.曜日取得 ./
    ?STEP :=   'ONCHECK'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-HCONTL1,TOFILE-HCONTL1.TOKKLIB
    CALL ONCHECK.TOKELIBO,PARA-(?YOUBI)
    ?PGMEC    :=    @PGMEC
    IF        ?PGMEC    ^=   0      THEN
              GOTO ABEND
    END

CRTID1: /.退避先編集 ZAMZAIF ./
    ?STEP :=   'CRTID1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?FILEID  :=  'ZAMZAIF'
    ?FILEN   :=  %NAME(?FILEID)

    ?LIBID   :=  ?LIBID1 && ?YOUBI
    ?LIBN    :=  %NAME(?LIBID)

    ?FILE    :=  %NCAT(?FILEN,?LIBN)

    ?FILETXT :=  %STRING(?FILE)
    ?MSGX :=  '* FILE-ID = ' && ?FILETXT && ' *'
    SNDMSG    ?MSGX,TO-XCTL

SETPF1: /.ﾃﾞｰﾀ退避 ZAMZAIF ./
    ?STEP :=   'SETPF1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SETPF FILE-ZAMZAIF.TOKFLIB,TOFILE-?FILE,ADD-@NO,ACTCHK-@NO
    ?PGMEC    :=    @PGMEC
    IF        ?PGMEC     =   0      THEN
              GOTO CRTID2
    END
    IF        ?PGMEC     =   0540   THEN
              SNDMSG '複写先ファイルなし！',TO-XCTL
              GOTO CPYFILE1
        ELSE
              GOTO ABEND
    END

CPYFILE1: /.ﾃﾞｰﾀ複写 ZAMZAIF ./
    ?STEP :=   'CPYFILE1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG    'ファイル複写　開始',TO-XCTL
    CPYFILE FILE-ZAMZAIF.TOKFLIB,TOFILE-?FILE,CRTFILE-@YES
    ?PGMEC    :=    @PGMEC
    IF        ?PGMEC    ^=   0      THEN
              GOTO ABEND
    END
    SNDMSG    'ファイル複写　完了',TO-XCTL

CRTID2: /.退避先編集 ZAMJISF ./
    ?STEP :=   'CRTID2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?FILEID  :=  'ZAMJISF'
    ?FILEN   :=  %NAME(?FILEID)

    ?LIBID   :=  ?LIBID1 && ?YOUBI
    ?LIBN    :=  %NAME(?LIBID)

    ?FILE    :=  %NCAT(?FILEN,?LIBN)

    ?FILETXT :=  %STRING(?FILE)
    ?MSGX :=  '* FILE-ID = ' && ?FILETXT && ' *'
    SNDMSG    ?MSGX,TO-XCTL

SETPF2: /.ﾃﾞｰﾀ退避 ZAMJISF ./
    ?STEP :=   'SETPF2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SETPF FILE-ZAMJISF.TOKFLIB,TOFILE-?FILE,ADD-@NO,ACTCHK-@NO
    ?PGMEC    :=    @PGMEC
    IF        ?PGMEC     =   0      THEN
              GOTO SZA0190B
    END
    IF        ?PGMEC     =   0540   THEN
              SNDMSG '複写先ファイルなし！',TO-XCTL
              GOTO CPYFILE2
        ELSE
              GOTO ABEND
    END

CPYFILE2: /.ﾃﾞｰﾀ複写 ZAMJISF ./
    ?STEP :=   'CPYFILE2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG    'ファイル複写　開始',TO-XCTL
    CPYFILE FILE-ZAMJISF.TOKFLIB,TOFILE-?FILE,CRTFILE-@YES
    ?PGMEC    :=    @PGMEC
    IF        ?PGMEC    ^=   0      THEN
              GOTO ABEND
    END
    SNDMSG    'ファイル複写　完了',TO-XCTL

/.↑ NASﾗｲﾌﾞﾗﾘへのﾃﾞｰﾀ退避ﾃｽﾄ--------------------------./

/.--------------------------------------------------------------./

/.  年次在庫データ繰り越し                                      ./
SZA0190B:
    ?STEP :=   'SZA0190B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.
    OVRF      FILE-ZAMJISF,TOFILE-ZAMJISF.TOKFLIB
    CALL      PGM-SZA0190B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
    ./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'


    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC

```
