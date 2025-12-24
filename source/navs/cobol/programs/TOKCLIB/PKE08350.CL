/. ***********************************************************  ./
/. *     サカタのタネ　出荷検品システム（本社システム）      *  ./
/. *   SYSTEM-NAME :    出荷検品システム　　　　　　　　　　 *  ./
/. *   JOB-ID      :    PKE02000                             *  ./
/. *   JOB-NAME    :    受信データ件数カウント               *  ./
/. ***********************************************************  ./
    PGM (P1-?SOKO,P2-?JOHO,P3-?LABH,P4-?LABM,P5-?RERR,P6-?KEKA)
/.##ﾊﾟﾗﾒﾀ定義##./
    PARA  ?SOKO  ,STRING*2,IN,VALUE-'  '      /.倉庫CD./
    PARA  ?JOHO  ,STRING*6,OUT,VALUE-'      ' /.出荷情報./
    PARA  ?LABH  ,STRING*6,OUT,VALUE-'      ' /.封筒情報./
    PARA  ?LABM  ,STRING*6,OUT,VALUE-'      ' /.梱包情報./
    PARA  ?RERR  ,STRING*6,OUT,VALUE-'      ' /.ｴﾗｰ 情報./
    PARA  ?KEKA  ,STRING*1,OUT,VALUE-' '      /.結果./
/.##ﾜｰｸ定義##./
    VAR   ?PGMEC ,INTEGER
    VAR   ?PGMECX,STRING*11
    VAR   ?PGMEM ,STRING*99
    VAR   ?MSG   ,STRING*99(6)
    VAR   ?MSGX  ,STRING*99
    VAR   ?PGMID ,STRING*8,VALUE-'PKE02000'
    VAR   ?STEP  ,STRING*8
    VAR   ?FILNM ,STRING*8,VALUE-'        ' /.ﾌｧｲﾙ名合併用./
    VAR   ?FILNMS,STRING*6,VALUE-'SYJOHO'   /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR   ?FILNMF,STRING*6,VALUE-'SYLABH'   /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR   ?FILNMK,STRING*6,VALUE-'SYLABM'   /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR   ?FILNME,STRING*6,VALUE-'RCVERR'   /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR   ?LIBNM ,STRING*8,VALUE-'TOKFLIB ' /.ﾗｲﾌﾞﾗﾘ名ﾃﾞﾌｫﾙﾄ./
    VAR   ?FILID ,NAME                      /.ﾌｧｲﾙ名名前型./
    VAR   ?LIBID ,NAME                      /.ﾗｲﾌﾞﾗﾘ名名前型./
    VAR   ?RCVSYUF ,NAME!MOD                /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR   ?RCVSYUFN,STRING*17               /.ﾌｧｲﾙ名表示用./
    VAR   ?RCVFUTF ,NAME!MOD                /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR   ?RCVFUTFN,STRING*17               /.ﾌｧｲﾙ名表示用./
    VAR   ?RCVKONF ,NAME!MOD                /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR   ?RCVKONFN,STRING*17               /.ﾌｧｲﾙ名表示用./
    VAR   ?RCVERRF ,NAME!MOD                /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR   ?RCVERRFN,STRING*17               /.ﾌｧｲﾙ名表示用./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    ?MSGX := '## 実行倉庫ｺｰﾄﾞ = ' && ?SOKO && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?FILNM    :=    ?FILNMS && ?SOKO      /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?RCVSYUF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?RCVSYUFN :=    %STRING(?RCVSYUF)
    ?MSGX     :=    '## 検品GF名 = ' && ?RCVSYUFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?FILNMF && ?SOKO      /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?RCVFUTF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?RCVFUTFN :=    %STRING(?RCVFUTF)
    ?MSGX     :=    '## 封筒F名  = ' && ?RCVFUTFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?FILNMK && ?SOKO      /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?RCVKONF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?RCVKONFN :=    %STRING(?RCVKONF)
    ?MSGX     :=    '## 梱包F名  = ' && ?RCVKONFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?FILNME && ?SOKO      /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?RCVERRF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?RCVERRFN :=    %STRING(?RCVERRF)
    ?MSGX     :=    '## ｴﾗｰF名   = ' && ?RCVERRFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##出荷検品関連Ｆ件数カウント##./
SKE0835B:

    ?STEP :=   'SKE0835B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SYJOHOF,TOFILE-?RCVSYUF
    OVRF      FILE-SYLABHF,TOFILE-?RCVFUTF
    OVRF      FILE-SYLABMF,TOFILE-?RCVKONF
    OVRF      FILE-RCVERRF,TOFILE-?RCVERRF
    CALL      PGM-SKE0835B.TOKELIB,PARA-(?JOHO,?LABH,?LABM,?RERR)
    IF        @PGMEC    ^=   0   THEN
              GOTO ABEND
    END
    SNDMSG ?JOHO,TO-XCTL.@ORGPROF
    SNDMSG ?LABH,TO-XCTL.@ORGPROF
    SNDMSG ?LABM,TO-XCTL.@ORGPROF
    SNDMSG ?RERR,TO-XCTL.@ORGPROF

RTN: /.##ﾌﾟﾛｸﾞﾗﾑ正常時、終了ﾒｯｾｰｼﾞ##./

    ?KEKA :=  '0'
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:/.##ﾌﾟﾛｸﾞﾗﾑ異常終了時、終了ﾒｯｾｰｼﾞ##./

    ?KEKA :=  '1'
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
