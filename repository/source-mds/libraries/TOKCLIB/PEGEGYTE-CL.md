# PEGEGYTE

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PEGEGYTE.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    営業所データ抽出                     *  ./
/. *   JOB-ID      :    PEGEGYTE                             *  ./
/. *   JOB-NAME    :    手書伝票データ再送信                 *  ./
/. ***********************************************************  ./
    PGM (P1-?P0)
    PARA ?P0      ,STRING*02,IN,VALUE-'  '      /.ｴｲｷﾞｮｳｼｮｺｰﾄﾞ./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?FILNM    ,STRING*8,VALUE-'        '    /.ﾌｧｲﾙ名合併用./
    VAR ?SPATHNM  ,STRING*7,VALUE-'       '     /.ﾊﾟｽ名合併用./
    VAR ?SNODENM  ,STRING*7,VALUE-'       '     /.ﾊﾟｽ名合併用./
    VAR ?FILNMH   ,STRING*6,VALUE-'EGYTEG'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILNMT   ,STRING*5,VALUE-'JHTTE'       /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?SPATHH   ,STRING*5,VALUE-'IHPAT'       /.ﾊﾟｽ名ﾃﾞﾌｫﾙﾄ./
    VAR ?SNODEH   ,STRING*5,VALUE-'IHNOD'       /.ﾉｰﾄﾞ名ﾃﾞﾌｫﾙﾄ./
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKFLIB '    /.ﾗｲﾌﾞﾗﾘ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILID    ,NAME                         /.ﾌｧｲﾙ名名前型./
    VAR ?LIBID    ,NAME                         /.ﾗｲﾌﾞﾗﾘ名名前型./
    VAR ?SPATHID  ,NAME                         /.ﾊﾟｽ名名前型./
    VAR ?SNODEID  ,NAME                         /.ﾉｰﾄﾞ名名前型./
    VAR ?JHTDENF  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?JHTTEGF  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?JHTDENFN ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?JHTTEGFN ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?SCOMNM   ,NAME!MOD                     /.ﾊﾟｽ.ﾉｰﾄﾞ./
    VAR ?SCOMNMF  ,STRING*15                    /.ﾊﾟｽ.ﾉｰﾄﾞ名表示用./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER
    VAR ?PGMECX   ,STRING*11
    VAR ?PGMEM    ,STRING*99
    VAR ?MSG      ,STRING*99(6)
    VAR ?MSGX     ,STRING*99
    VAR ?PGMID    ,STRING*8,VALUE-'PEG00800'
    VAR ?STEP     ,STRING*8
    VAR ?OPR1     ,STRING*50                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2     ,STRING*50                    /.      2    ./
    VAR ?OPR3     ,STRING*50                    /.      3    ./
    VAR ?OPR4     ,STRING*50                    /.      4    ./
    VAR ?OPR5     ,STRING*50                    /.      5    ./

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.## ﾒｯｾｰｼﾞ ##./
    ?OPR1  :=  '　＃＃＃＃＃＃＃　本社データ連携　＃＃＃＃＃＃＃　'
    ?OPR2  :=  '　　手書伝票データの再送信処理を行ないます。　　　'
    ?OPR3  :=  '　　最後に抽出した取引先のデータが対象です。　　　'
    ?OPR4  :=  '　　確認して下さい。　　　　　　　　　　　　　　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    ?MSGX := '## 実行営業所ｺｰﾄﾞ = ' && ?P0 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?FILNM    :=    ?FILNMH && ?P0        /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?JHTDENF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?JHTDENFN :=    %STRING(?JHTDENF)
    ?MSGX     :=    '## 倉庫F名(SF) = ' && ?JHTDENFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##パス名＋ノード名取得##./

    ?SPATHNM  :=    ?SPATHH && ?P0
    ?SNODENM  :=    ?SNODEH && ?P0
    ?SPATHID  :=    %NAME(?SPATHNM)
    ?SNODEID  :=    %NAME(?SNODENM)
    ?SCOMNM   :=    %NCAT(?SPATHID,?SNODEID)
    ?SCOMNMF  :=    %STRING(?SCOMNM)
    ?MSGX     :=    '## 送信ﾊﾟｽ名    = ' && ?SCOMNMF && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##手書伝票データ送信##./
SNDFILE1:

    ?STEP :=   'SNDFILE1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX     :=    '## ﾃｶﾞｷﾃﾞﾝﾋﾟｮｳ送信中   ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SNDFILE COM-?SCOMNM,SFILE-?JHTDENF,DFILE-'EGYTEGF',RL-1020,BF-1
    IF        @PGMEC    ^=   0
          THEN
              ?MSGX     :=    '## ﾃｶﾞｷﾃﾞﾝﾋﾟｮｳ送信異常 ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO ABEND
          ELSE
              ?MSGX     :=    '## ﾃｶﾞｷﾃﾞﾝﾋﾟｮｳ送信完了 ##'
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
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC

```
