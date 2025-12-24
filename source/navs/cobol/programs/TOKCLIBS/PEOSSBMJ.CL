/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受配信管理システム                   *  ./
/. *   JOB-ID      :    PEOSSBMJ                             *  ./
/. *   JOB-NAME    :    ２サーバー　受信起動振分　　　　　　 *  ./
/. *   優先１＝＞ＰＧサーバー　優先２＝＞Ｋサーバー          *  ./
/. ***********************************************************  ./
    PGM (P1-?HIDUKE,P2-?JIKAN,P3-?TOKCD,P4-?PASS,P5-?HENID,
         P6-?LINE,P7-?YUSEN,P8-?LIBNM,P9-?FILNM,P10-?KJOB,P11-?JRL,
         P12-?JYUKBN)
/.##ﾊﾟﾗﾒﾀ定義##./
    PARA      ?HIDUKE   ,STRING*8,IN,VALUE-'        ' /.受信日付./
    PARA      ?JIKAN    ,STRING*4,IN,VALUE-'    '     /.受信時間./
    PARA      ?TOKCD    ,STRING*8,IN,VALUE-'        ' /.受信取引先./
    PARA      ?PASS     ,STRING*13,IN,VALUE-'             '
    PARA      ?HENID    ,STRING*8,IN,VALUE-'        ' /.変換ID./
    PARA      ?LINE     ,STRING*1,IN,VALUE-' '        /.回線./
    PARA      ?YUSEN    ,STRING*1,IN,VALUE-' '        /.回線優先./
    PARA      ?LIBNM    ,STRING*8,IN,VALUE-'        ' /.集信LIB./
    PARA      ?FILNM    ,STRING*8,IN,VALUE-'        ' /.集信FILE./
    PARA      ?KJOB     ,STRING*8,IN,VALUE-'        ' /.起動JOB./
    PARA      ?JRL      ,STRING*4,IN,VALUE-'    '     /.ﾚｺｰﾄﾞ長./
    PARA      ?JYUKBN   ,STRING*1,IN,VALUE-' '        /.受信区分./
/.##ﾜｰｸﾃｲｷﾞ##./
    VAR       ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR       ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR       ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR       ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR       ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR       ?PGMID    ,STRING*8,VALUE-'PSJH1101'  /.PROGRAM-ID./
    VAR       ?STEP     ,STRING*8                   /.STEP-ID./
/.##ｹｯｶFLG##./
    VAR       ?KEKA     ,STRING*2,VALUE-'  '        /.更新ｹｯｶFLG./

/.##ﾌﾟﾛｸﾞﾗﾑｶｲｼﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##DEFLIBL##./
    DEFLIBL TOKELIB/TOKFLIB
/.##ﾊﾟﾗﾒﾀﾌｧｲﾙ作成##./
JOBSTR:

    ?STEP :=   'JOBSTR  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    /.##ＩＮＳ回線＋優先順位１の場合##./
    IF ?LINE = 'I' & ?YUSEN = '1'  THEN
       GOTO  PGINS1
    END
    /.##ＩＮＳ回線＋優先順位２の場合##./
    IF ?LINE = 'I' & ?YUSEN = '2'  THEN
       GOTO  K6INS2
    END
    /.##ＩＮＳ回線＋優先順位３の場合##./
    IF ?LINE = 'I' & ?YUSEN = '3'  THEN
       GOTO  K6INS3
    END
    /.##電話回線＋優先順位１の場合##./
    IF ?LINE = 'T' & ?YUSEN = '1'  THEN
       GOTO  PGTEL1
    END
    /.##電話回線＋優先順位２の場合##./
    IF ?LINE = 'T' & ?YUSEN = '2'  THEN
       GOTO  K6TEL2
    END
    /.##電話回線＋優先順位３の場合##./
    IF ?LINE = 'T' & ?YUSEN = '3'  THEN
       GOTO  K6TEL3
    END
/.##ＰＣ回線＋優先順位１の場合##./
    IF ?LINE = 'P' & ?YUSEN = '1'  THEN
       GOTO  PCINS1
    END

PGINS1: /.##PG6000 INS ｼﾞｮﾌﾞ起動##./

    ?STEP :=   'PGINS1  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

 /.##受信ｽﾀｰﾄﾒｯｾｰｼﾞ##./
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## INS-1 ｼﾞｭｼﾝJOB START ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## FEP1-SVR             ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
 /.##ﾊﾟﾗﾒﾀﾌｧｲﾙ作成##./
    OVRF FILE-PARAFILE,TOFILE-PARAINS2.TOKFLIB
    CALL PGM-SCV0100B.TOKELIB,
         PARA-(?HIDUKE,?JIKAN,?TOKCD,?PASS,?HENID,?LINE,?YUSEN,
               ?LIBNM,?FILNM,?KJOB,?JRL,?JYUKBN)
    IF @PGMEC  ^=  0  THEN
       ?KEKA  :=  '80'
       OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
       CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
       GOTO  ABEND
    END
 /.##ﾊﾟﾗﾒﾀﾌｧｲﾙｿｳｼﾝ##./
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## ﾊﾟﾗﾒﾀﾌｧｲﾙ ｿｳｼﾝ START ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDFILE COM-KGPATH03.KGNODE03,SFILE-PARAINS2.TOKFLIB,
            DFILE-'PARAINS1',RL-150,BF-1
    IF @PGMEC  ^=  0  THEN
       ?KEKA  :=  '81'
       OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
       CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
       GOTO  ABEND
    END
 /.##GP6000側JOB起動##./
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## FEP1   JOB(INS)START ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    CALL TWAIT30.XUCL /.##受信ｼﾞｮﾌﾞ起動を30秒待合せる##./
    SNDRSJJ COM-KGPATH03.KGNODE03,DFILE-'CVCSINS1'
    IF @PGMEC  ^=  0  THEN
       ?KEKA  :=  '82'
       OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
       CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
       GOTO  ABEND
    END
 /.##ｼﾞｮﾌﾞ正常終了へ##./
    GOTO  RTN

K6INS2: /.##K6500 INS  ｼﾞｮﾌﾞ起動##./

    ?STEP :=   'K6INS2  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

 /.##受信ｽﾀｰﾄﾒｯｾｰｼﾞ##./
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## INS-2 ｼﾞｭｼﾝJOB START ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## FEP2-SVR             ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
 /.##ﾊﾟﾗﾒﾀﾌｧｲﾙ作成##./
    OVRF FILE-PARAFILE,TOFILE-PARAINS1.TOKFLIB
    CALL PGM-SCV0100B.TOKELIB,
         PARA-(?HIDUKE,?JIKAN,?TOKCD,?PASS,?HENID,?LINE,?YUSEN,
               ?LIBNM,?FILNM,?KJOB,?JRL,?JYUKBN)
    IF @PGMEC  ^=  0  THEN
       ?KEKA  :=  '80'
       OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
       CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
       GOTO  ABEND
    END
 /.##ﾊﾟﾗﾒﾀﾌｧｲﾙｿｳｼﾝ##./
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## ﾊﾟﾗﾒﾀﾌｧｲﾙ ｿｳｼﾝ START ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDFILE COM-KGPATH01.KGNODE01,SFILE-PARAINS1.TOKFLIB,
            DFILE-'PARAINS1',RL-150,BF-1
    IF @PGMEC  ^=  0  THEN
       ?KEKA  :=  '81'
       OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
       CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
       GOTO  ABEND
    END
 /.##GP6000側JOB起動##./
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## FEP2   JOB(INS)START ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    CALL TWAIT30.XUCL /.##受信ｼﾞｮﾌﾞ起動を30秒待合せる##./
    SNDRSJJ COM-KGPATH01.KGNODE01,DFILE-'CVCSINS1'
    IF @PGMEC  ^=  0  THEN
       ?KEKA  :=  '82'
       OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
       CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
       GOTO  ABEND
    END
 /.##ｼﾞｮﾌﾞ正常終了へ##./
    GOTO  RTN

K6INS3: /.##岡山  INS  ｼﾞｮﾌﾞ起動##./

    ?STEP :=   'K6INS3  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

 /.##受信ｽﾀｰﾄﾒｯｾｰｼﾞ##./
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## INS-3 ｼﾞｭｼﾝJOB START ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## FEP3-SVR             ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
 /.##ﾊﾟﾗﾒﾀﾌｧｲﾙ作成##./
    OVRF FILE-PARAFILE,TOFILE-PARAINS3.TOKFLIB
    CALL PGM-SCV0100B.TOKELIB,
         PARA-(?HIDUKE,?JIKAN,?TOKCD,?PASS,?HENID,?LINE,?YUSEN,
               ?LIBNM,?FILNM,?KJOB,?JRL,?JYUKBN)
    IF @PGMEC  ^=  0  THEN
       ?KEKA  :=  '80'
       OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
       CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
       GOTO  ABEND
    END
 /.##ﾊﾟﾗﾒﾀﾌｧｲﾙｿｳｼﾝ##./
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## ﾊﾟﾗﾒﾀﾌｧｲﾙ ｿｳｼﾝ START ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDFILE COM-KGPATH04.KGNODE04,SFILE-PARAINS3.TOKFLIB,
            DFILE-'PARAINS1',RL-150,BF-1
    IF @PGMEC  ^=  0  THEN
       ?KEKA  :=  '81'
       OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
       CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
       GOTO  ABEND
    END
 /.##GP6000側JOB起動##./
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## FEP3   JOB(INS)START ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    CALL TWAIT30.XUCL /.##受信ｼﾞｮﾌﾞ起動を30秒待合せる##./
    SNDRSJJ COM-KGPATH04.KGNODE04,DFILE-'CVCSINS1'
    IF @PGMEC  ^=  0  THEN
       ?KEKA  :=  '82'
       OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
       CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
       GOTO  ABEND
    END
 /.##ｼﾞｮﾌﾞ正常終了へ##./
    GOTO  RTN

PGTEL1: /.##PG6000 TEL ｼﾞｮﾌﾞ起動##./

    ?STEP :=   'PGTEL1  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

 /.##受信ｽﾀｰﾄﾒｯｾｰｼﾞ##./
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## TEL-1 ｼﾞｭｼﾝJOB START ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## FEP1-SVR             ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
 /.##ﾊﾟﾗﾒﾀﾌｧｲﾙ作成##./
    OVRF FILE-PARAFILE,TOFILE-PARATEL2.TOKFLIB
    CALL PGM-SCV0100B.TOKELIB,
         PARA-(?HIDUKE,?JIKAN,?TOKCD,?PASS,?HENID,?LINE,?YUSEN,
               ?LIBNM,?FILNM,?KJOB,?JRL,?JYUKBN)
    IF @PGMEC  ^=  0  THEN
       ?KEKA  :=  '80'
       OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
       CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
       GOTO  ABEND
    END
 /.##ﾊﾟﾗﾒﾀﾌｧｲﾙｿｳｼﾝ##./
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## ﾊﾟﾗﾒﾀﾌｧｲﾙ ｿｳｼﾝ START ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDFILE COM-KGPATH03.KGNODE03,SFILE-PARATEL2.TOKFLIB,
            DFILE-'PARATEL1',RL-150,BF-1
    IF @PGMEC  ^=  0  THEN
       ?KEKA  :=  '81'
       OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
       CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
       GOTO  ABEND
    END
 /.##GP6000側JOB起動##./
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## FEP1   JOB(TEL)START ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    CALL TWAIT30.XUCL /.##受信ｼﾞｮﾌﾞ起動を30秒待合せる##./
    SNDRSJJ COM-KGPATH03.KGNODE03,DFILE-'CVCSTEL1'
    IF @PGMEC  ^=  0  THEN
       ?KEKA  :=  '82'
       OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
       CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
       GOTO  ABEND
    END
 /.##ｼﾞｮﾌﾞ正常終了へ##./
    GOTO  RTN

K6TEL2: /.##K6500 INS  ｼﾞｮﾌﾞ起動##./

    ?STEP :=   'K6TEL2  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

 /.##受信ｽﾀｰﾄﾒｯｾｰｼﾞ##./
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## TEL-2 ｼﾞｭｼﾝJOB START ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## FEP2-SVR             ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
 /.##ﾊﾟﾗﾒﾀﾌｧｲﾙ作成##./
    OVRF FILE-PARAFILE,TOFILE-PARATEL1.TOKFLIB
    CALL PGM-SCV0100B.TOKELIB,
         PARA-(?HIDUKE,?JIKAN,?TOKCD,?PASS,?HENID,?LINE,?YUSEN,
               ?LIBNM,?FILNM,?KJOB,?JRL,?JYUKBN)
    IF @PGMEC  ^=  0  THEN
       ?KEKA  :=  '80'
       OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
       CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
       GOTO  ABEND
    END
 /.##ﾊﾟﾗﾒﾀﾌｧｲﾙｿｳｼﾝ##./
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## ﾊﾟﾗﾒﾀﾌｧｲﾙ ｿｳｼﾝ START ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDFILE COM-KGPATH01.KGNODE01,SFILE-PARATEL1.TOKFLIB,
            DFILE-'PARATEL1',RL-150,BF-1
    IF @PGMEC  ^=  0  THEN
       ?KEKA  :=  '81'
       OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
       CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
       GOTO  ABEND
    END
 /.##GP6000側JOB起動##./
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## FEP2   JOB(INS)START ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    CALL TWAIT30.XUCL /.##受信ｼﾞｮﾌﾞ起動を30秒待合せる##./
    SNDRSJJ COM-KGPATH01.KGNODE01,DFILE-'CVCSTEL1'
    IF @PGMEC  ^=  0  THEN
       ?KEKA  :=  '82'
       OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
       CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
       GOTO  ABEND
    END
 /.##ｼﾞｮﾌﾞ正常終了へ##./
    GOTO  RTN

K6TEL3: /.##岡山  INS  ｼﾞｮﾌﾞ起動##./

    ?STEP :=   'K6TEL3  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

 /.##受信ｽﾀｰﾄﾒｯｾｰｼﾞ##./
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## TEL-3 ｼﾞｭｼﾝJOB START ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## FEP3-SVR             ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
 /.##ﾊﾟﾗﾒﾀﾌｧｲﾙ作成##./
    OVRF FILE-PARAFILE,TOFILE-PARATEL3.TOKFLIB
    CALL PGM-SCV0100B.TOKELIB,
         PARA-(?HIDUKE,?JIKAN,?TOKCD,?PASS,?HENID,?LINE,?YUSEN,
               ?LIBNM,?FILNM,?KJOB,?JRL,?JYUKBN)
    IF @PGMEC  ^=  0  THEN
       ?KEKA  :=  '80'
       OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
       CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
       GOTO  ABEND
    END
 /.##ﾊﾟﾗﾒﾀﾌｧｲﾙｿｳｼﾝ##./
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## ﾊﾟﾗﾒﾀﾌｧｲﾙ ｿｳｼﾝ START ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDFILE COM-KGPATH04.KGNODE04,SFILE-PARATEL3.TOKFLIB,
            DFILE-'PARATEL1',RL-150,BF-1
    IF @PGMEC  ^=  0  THEN
       ?KEKA  :=  '81'
       OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
       CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
       GOTO  ABEND
    END
 /.##GP6000側JOB起動##./
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'## FEP3   JOB(INS)START ##',TO-XCTL,JLOG-@YES
    SNDMSG MSG-'##########################',TO-XCTL,JLOG-@YES
    CALL TWAIT30.XUCL /.##受信ｼﾞｮﾌﾞ起動を30秒待合せる##./
    SNDRSJJ COM-KGPATH04.KGNODE04,DFILE-'CVCSTEL1'
    IF @PGMEC  ^=  0  THEN
       ?KEKA  :=  '82'
       OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
       CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
       GOTO  ABEND
    END
 /.##ｼﾞｮﾌﾞ正常終了へ##./
    GOTO  RTN

PCINS1: /.##全銀TCP/IP ｼﾞｮﾌﾞ起動##./

    SNDMSG MSG-'##########################',TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG MSG-'## PC-1  ｼﾞｭｼﾝJOB START ##',TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG MSG-'##########################',TO-XCTL.@ORGPROF,JLOG-@YES
 /.##ﾊﾟﾗﾒﾀﾌｧｲﾙ作成##./
    OVRF FILE-PARAFILE,TOFILE-PARAPC1.TOKFLIB
    CALL PGM-SCV0100B.TOKELIB,
         PARA-(?HIDUKE,?JIKAN,?TOKCD,?PASS,?HENID,?LINE,?YUSEN,
               ?LIBNM,?FILNM,?KJOB,?JRL,?JYUKBN)
    IF   @PGMEC  ^=  0  THEN
         ?KEKA  :=  '50'
         OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
         CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
         GOTO  ABEND
    END
 /.##受信待ち受けJOB起動##./
    SNDMSG MSG-'##########################',TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG MSG-'## PC    JOB      START ##',TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG MSG-'##########################',TO-XCTL.@ORGPROF,JLOG-@YES
    SBMJOB JOB-CVCSPC,JOBD-CVCS.XUCL,JOBK-@B,
            PGM-CVCSPC1.TOKELIB,EPGM-ERRPC1.TOKELIB,LOG-@YES!
            1024,PGMEL-@I/@L/@S/@T,LIBL-TOKELIB/TOKFLIB
    IF   @PGMEC  ^=  0  THEN
         ?KEKA  :=  '51'
         OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
         CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
         GOTO  ABEND
    END
 /.##ｼﾞｮﾌﾞ正常終了へ##./
    GOTO  RTN

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    RETURN    PGMEC-@PGMEC

/.##異常終了時##./
ABEND:
    /.##異常終了時，回線クリア##./
    OVRF FILE-JHMKAIL1,TOFILE-JHMKAIL1.TOKFLIB
    CALL PGM-SCV0120B.TOKELIB,PARA-(?LINE,?YUSEN)

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
    SNDMSG ?PGMEM,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC
