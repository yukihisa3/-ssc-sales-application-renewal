# PEGZAIKO

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PEGZAIKO.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    営業所データ連携                     *  ./
/. *   JOB-ID      :    PEGZAIKO                             *  ./
/. *   JOB-NAME    :    在庫データ再送信                     *  ./
/. ***********************************************************  ./
    PGM (P1-?EIGYO)
/.##ﾊﾟﾗﾒﾀ##./
    PARA ?EIGYO   ,STRING*2,IN,VALUE-'00'       /.ﾊﾟﾗﾒﾀｴｲｷﾞｮｳｼｮ./
/.##ﾜｰｸｴﾘｱ##./
    VAR ?FILNM    ,STRING*8,VALUE-'        '    /.ﾌｧｲﾙ名合併用./
    VAR ?SPATHNM  ,STRING*7,VALUE-'       '     /.ﾊﾟｽ名合併用./
    VAR ?SNODENM  ,STRING*7,VALUE-'       '     /.ﾊﾟｽ名合併用./
    VAR ?FILNMH   ,STRING*6,VALUE-'EGYZAI'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?SPATHH   ,STRING*5,VALUE-'IHPAT'       /.ﾊﾟｽ名ﾃﾞﾌｫﾙﾄ./
    VAR ?SNODEH   ,STRING*5,VALUE-'IHNOD'       /.ﾉｰﾄﾞ名ﾃﾞﾌｫﾙﾄ./
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKFLIB '    /.ﾗｲﾌﾞﾗﾘ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILID    ,NAME                         /.ﾌｧｲﾙ名名前型./
    VAR ?LIBID    ,NAME                         /.ﾗｲﾌﾞﾗﾘ名名前型./
    VAR ?SPATHID  ,NAME                         /.ﾊﾟｽ名名前型./
    VAR ?SNODEID  ,NAME                         /.ﾉｰﾄﾞ名名前型./
    VAR ?ZAIKOF   ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?ZAIKOFN  ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?SCOMNM   ,NAME!MOD                     /.ﾊﾟｽ.ﾉｰﾄﾞ./
    VAR ?SCOMNMF  ,STRING*15                    /.ﾊﾟｽ.ﾉｰﾄﾞ名表示用./
    VAR ?PGMEC    ,INTEGER
    VAR ?PGMECX   ,STRING*11
    VAR ?PGMEM    ,STRING*99
    VAR ?MSG      ,STRING*99(6)
    VAR ?MSGX     ,STRING*99
    VAR ?PGMID    ,STRING*8,VALUE-'PEG01100'
    VAR ?STEP     ,STRING*8
    VAR ?WKSTN    ,STRING*8
    VAR ?OPR1     ,STRING*50                    /.ﾒｯｾｰｼﾞ1./
    VAR ?OPR2     ,STRING*50                    /.      2./
    VAR ?OPR3     ,STRING*50                    /.      3./
    VAR ?OPR4     ,STRING*50                    /.      4./
    VAR ?OPR5     ,STRING*50                    /.      5./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.## ﾒｯｾｰｼﾞ ##./
    ?OPR1  :=  '　＃＃＃＃＃＃＃　本社データ連携　＃＃＃＃＃＃＃　'
    ?OPR2  :=  '　　営業所在庫データの再送信処理を行ないます。　　'
    ?OPR3  :=  '　　在庫に抽出した在庫データが対象です。　　　　　'
    ?OPR4  :=  '　　確認して下さい。　　　　　　　　　　　　　　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    ?MSGX := '## 営業所ｺｰﾄﾞ   = ' && ?EIGYO && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?FILNM    :=    ?FILNMH && ?EIGYO     /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?ZAIKOF   :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?ZAIKOFN  :=    %STRING(?ZAIKOF)
    ?MSGX     :=    '## 営業所ﾌｧｲﾙ名 = ' && ?ZAIKOFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##パス名＋ノード名取得##./

    ?SPATHNM  :=    ?SPATHH && ?EIGYO
    ?SNODENM  :=    ?SNODEH && ?EIGYO
    ?SPATHID  :=    %NAME(?SPATHNM)
    ?SNODEID  :=    %NAME(?SNODENM)
    ?SCOMNM   :=    %NCAT(?SPATHID,?SNODEID)
    ?SCOMNMF  :=    %STRING(?SCOMNM)
    ?MSGX     :=    '## 送信ﾊﾟｽ名    = ' && ?SCOMNMF && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##在庫データ再送信##./
SNDFILE1:

    ?STEP :=   'SNDFILE1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX     :=    '## 在庫ﾃﾞｰﾀ 再送信中   ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SNDFILE COM-?SCOMNM,SFILE-?ZAIKOF,DFILE-'EGYZAIF',RL-600,BF-1
    IF        @PGMEC    ^=   0
          THEN
              ?MSGX     :=    '## 在庫ﾃﾞｰﾀ 再送信異常 ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO ABEND
          ELSE
              ?MSGX     :=    '## 在庫ﾃﾞｰﾀ 再送信完了 ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END

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
