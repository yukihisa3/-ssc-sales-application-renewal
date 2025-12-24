# PKE0200N

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PKE0200N.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　出荷検品システム（本社システム）      *  ./
/. *   SYSTEM-NAME :    出荷検品システム　　　　　　　　　　 *  ./
/. *   JOB-ID      :    PKE02000                             *  ./
/. *   JOB-NAME    :    受信データ更新／累積処理             *  ./
/. *   2002/05/01  片岡配送ｾﾝﾀｰ追加                          *  ./
/. ***********************************************************  ./
    PGM

    VAR   ?WS    ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR   ?WKSTN ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR   ?PGMEC ,INTEGER
    VAR   ?PGMECX,STRING*11
    VAR   ?PGMEM ,STRING*99
    VAR   ?MSG   ,STRING*99(6)
    VAR   ?MSGX  ,STRING*99
    VAR   ?PGMID ,STRING*8,VALUE-'PKE02000'
    VAR   ?STEP  ,STRING*8
    VAR   ?P1    ,STRING*6,VALUE-'000000'   /.検品明細Ｆ./
    VAR   ?P2    ,STRING*6,VALUE-'000000'   /.累積検品明細Ｆ./
    VAR   ?P3    ,STRING*6,VALUE-'000000'   /.出荷ﾗﾍﾞﾙﾍｯﾀﾞＦ./
    VAR   ?P4    ,STRING*6,VALUE-'000000'   /.累積出荷ﾗﾍﾞﾙﾍｯﾀﾞＦ./
    VAR   ?P5    ,STRING*6,VALUE-'000000'   /.出荷ﾗﾍﾞﾙ明細Ｆ./
    VAR   ?P6    ,STRING*6,VALUE-'000000'   /.累積出荷ﾗﾍﾞﾙ明細Ｆ./
    VAR   ?P7    ,STRING*2,VALUE-'00'       /.代表倉庫./
    VAR   ?P8    ,STRING*2,VALUE-'00'       /.制御倉庫./
    VAR   ?FILNM ,STRING*8,VALUE-'        ' /.ﾌｧｲﾙ名合併用./
    VAR   ?FILNMS,STRING*6,VALUE-'SYJOHO'   /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR   ?FILNMF,STRING*6,VALUE-'SYLABM'   /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR   ?FILNMK,STRING*6,VALUE-'SYLABH'   /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR   ?FILNME,STRING*6,VALUE-'RCVERR'   /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR   ?LIBNM ,STRING*8,VALUE-'TOKWLIB ' /.ﾗｲﾌﾞﾗﾘ名ﾃﾞﾌｫﾙﾄ./
    VAR   ?LIBNMB,STRING*8,VALUE-'TOKBLIB ' /.ﾗｲﾌﾞﾗﾘ名ﾃﾞﾌｫﾙﾄBACKUP./
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
    VAR   ?RCVSYUFB,NAME!MOD                /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR   ?RCVFUTFB,NAME!MOD                /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR   ?RCVKONFB,NAME!MOD                /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR   ?SYJOHOF ,STRING*6,VALUE-'000000'
    VAR   ?SYLABHF ,STRING*6,VALUE-'000000'
    VAR   ?SYLABMF ,STRING*6,VALUE-'000000'
    VAR   ?RCVERRF1,STRING*6,VALUE-'000000'
    VAR   ?PDATE   ,STRING*8,VALUE-'00000000'
    VAR   ?PTIME   ,STRING*4,VALUE-'0000'
    VAR   ?SKEKA   ,STRING*1,VALUE-'0'
    VAR   ?PGNM    ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR   ?KEKA1   ,STRING*40                  /.      2    ./
    VAR   ?KEKA2   ,STRING*40                  /.      3    ./
    VAR   ?KEKA3   ,STRING*40                  /.      4    ./
    VAR   ?KEKA4   ,STRING*40                  /.      5    ./
    VAR   ?OPR1    ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR   ?OPR2    ,STRING*50                  /.      2    ./
    VAR   ?OPR3    ,STRING*50                  /.      3    ./
    VAR   ?OPR4    ,STRING*50                  /.      4    ./
    VAR   ?OPR5    ,STRING*50                  /.      5    ./

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '出荷検品－検品済ＤＴホスト更新'

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.#################################################################./
/.##資源の事前獲得（下記ファイル使用時、メッセージ表示）         ##./
/.#################################################################./
FILECHK:
    ASSIGN FILE-RUISYUF.TOKKLIB!@XCL/   /.##出荷検品情報ﾌｧｲﾙ##./
                RUIKONF.TOKKLIB!@XCL    /.##出荷検品梱包ﾌｧｲﾙ##./
    IF     @PGMEC  ^=    0    THEN
           ?OPR1  :=  '＜他倉庫使用中＞'
           ?OPR2  :=  '現在、他倉庫にて検品取込処理中です。'
           ?OPR3  :=  'もう暫く時間をおいてから再度実行して下さい。'
           ?OPR4  :=  ''
           ?OPR5  :=  ''
           CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
           RETURN
    END

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P7,?P8)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【倉庫ＣＤ取得】'
              GOTO ABEND
    END

/.##倉庫ｺｰﾄﾞ選択##./
SKY1701I:

    ?STEP :=   'SKY1701I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    CALL      PGM-SKY1702I.TOKELIB,PARA-(?P7,?P8)
    IF        @PGMEC    ^=   0    THEN
              ?PGMEC := @PGMEC
              IF    ?PGMEC  =  4010
                 THEN
                    ?MSGX := '##取消終了##'
                    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                    RETURN PGMEC-@PGMEC
                 ELSE
                    ?KEKA4 :=  '【倉庫ＣＤ入力】'
                    GOTO   ABEND
                 END
    END

/.##片岡特別処理##./
KATAOKAP:
/.## 旧倉庫ＣＤ変換処理を変更
    IF        ?P7  =  '6A'   THEN
              ?P7 :=  '62'
    END
##./
/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    ?MSGX := '## 実行倉庫ｺｰﾄﾞ = ' && ?P7 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?FILNM    :=    ?FILNMS && ?P7        /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?RCVSYUF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?LIBID    :=    %NAME(?LIBNMB)        /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?RCVSYUFB :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?RCVSYUFN :=    %STRING(?RCVSYUF)
    ?MSGX     :=    '## 検品GF名 = ' && ?RCVSYUFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?FILNMK && ?P7        /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?RCVFUTF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?LIBID    :=    %NAME(?LIBNMB)        /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?RCVFUTFB :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?RCVFUTFN :=    %STRING(?RCVFUTF)
    ?MSGX     :=    '## 封筒F名  = ' && ?RCVFUTFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?FILNMF && ?P7        /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?RCVKONF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?LIBID    :=    %NAME(?LIBNMB)        /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?RCVKONFB :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?RCVKONFN :=    %STRING(?RCVKONF)
    ?MSGX     :=    '## 梱包F名  = ' && ?RCVKONFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?FILNME && ?P7        /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?RCVERRF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?RCVERRFN :=    %STRING(?RCVERRF)
    ?MSGX     :=    '## ｴﾗｰF名   = ' && ?RCVERRFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ファイルバックアップ（フバサミ対応）##./
PBACKUP1:

    ?STEP :=   'PBACKUP1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-?RCVSYUF,TOFILE-?RCVSYUFB,
            ADD-@NO,BF-42
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【検品Ｆバックアップ】'
              GOTO ABEND
    END

/.##ファイルバックアップ##./
PBACKUP2:

    ?STEP :=   'PBACKUP2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-?RCVKONF,TOFILE-?RCVKONFB,
            ADD-@NO,BF-42
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【梱包Ｆバックアップ】'
              GOTO ABEND
    END

/.##ファイルバックアップ##./
PBACKUP3:

    ?STEP :=   'PBACKUP3'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-?RCVFUTF,TOFILE-?RCVFUTFB,
            ADD-@NO,BF-42
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【封筒Ｆバックアップ】'
              GOTO ABEND
    END

/.##ファイル件数カウント##./
SKE0835B:

    ?STEP :=   'SKE0835B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SYJOHOF,TOFILE-?RCVSYUF
    OVRF      FILE-SYLABHF,TOFILE-?RCVKONF
    OVRF      FILE-SYLABMF,TOFILE-?RCVFUTF
    OVRF      FILE-RCVERRF,TOFILE-?RCVERRF
    CALL      PGM-SKE0835B.TOKELIB,PARA-(?SYJOHOF,?SYLABHF,
                                         ?SYLABMF,?RCVERRF1)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【ファイル件数カウント】'
              GOTO ABEND
    END

/.##管理ファイル作成／更新##./
SKE0810B:

    ?STEP :=   'SKE0810B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-KANKENL1,TOFILE-KENKANL1.TOKKLIB
    CALL      PGM-SKE0810B.TOKELIB,PARA-(?P7,?SYJOHOF,?SYLABHF,
                                         ?SYLABMF,?RCVERRF1,
                                         ?PDATE,?PTIME)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【出荷検品管理Ｆ作成】'
              GOTO ABEND
    END

/.##前回分エラーファイルＣＯＰＹ##./
PCNVFILE:

    ?STEP :=   'PCNVFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-?RCVERRF,TOFILE-?RCVSYUF,BF-42
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【前回エラーＣＯＰＹ】'
              ?SKEKA := '1'
              GOTO ABEND1
    END

/.##検品明細更新累積##./
SKE0200B:

    ?STEP :=   'SKE0200B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-RCVSYUF,TOFILE-?RCVSYUF
    OVRF      FILE-RUISYUF,TOFILE-RUISYUL1.TOKKLIB
    OVRF      FILE-SETGKFG,TOFILE-SETGFKL1.TOKFLIB
    OVRF      FILE-ZAMZAIL1,TOFILE-ZAMZAIL1.TOKFLIB
    OVRF      FILE-SHTDENL1,TOFILE-SHTDENL1.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-RCVERRF,TOFILE-?RCVERRF
    CALL      PGM-SKE0200B.TOKELIB,PARA-(?P1,?P2)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【出荷検品ＤＴ更新】'
              ?SKEKA := '2'
              GOTO ABEND1
    END

/.##出荷ラベルヘッダ更新累積##./
SKE0210B:

    ?STEP :=   'SKE0210B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-RCVFUTF,TOFILE-?RCVKONF
    OVRF      FILE-RUIFUTF,TOFILE-RUIFUTL1.TOKFLIB
/.  CALL      PGM-SKE0210B.TOKELIB,PARA-(?P3,?P4) ./
    IF        @PGMEC    ^=   0   THEN
              ?SKEKA := '3'
              GOTO ABEND1
    END

/.##出荷ラベル明細更新累積##./
SKE0220B:

    ?STEP :=   'SKE0220B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-RCVKONF,TOFILE-?RCVFUTF
    OVRF      FILE-RUIKONF,TOFILE-RUIKONL1.TOKKLIB
    CALL      PGM-SKE0220B.TOKELIB,PARA-(?P5,?P6)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【梱包ラベルＤＴ更新】'
              ?SKEKA := '4'
              GOTO ABEND1
    END

/.##取込／累積件数リスト##./
SKE0230L:

    ?STEP :=   'SKE0230L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-SKE0230L.TOKELIB,PARA-(?P1,?P2,?P3,?P4,?P5,?P6)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【件数リスト発行】'
              ?SKEKA := '5'
              GOTO ABEND1
    END

/.##受信出荷情報Ｆ初期化##./
PCLR1:

    ?STEP :=   'PCLR1   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-?RCVSYUF
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【出荷検品ＤＴ初期化】'
              ?SKEKA := '6'
              GOTO ABEND1
    END

/.##受信封筒情報Ｆ初期化##./
PCLR2:

    ?STEP :=   'PCLR2   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-?RCVFUTF
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【封筒ラベルＤＴ初期化】'
              ?SKEKA := '7'
              GOTO ABEND1
    END

/.##受信梱包情報Ｆ初期化##./
PCLR3:

    ?STEP :=   'PCLR3   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-?RCVKONF
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【梱包ラベルＤＴ初期化】'
              ?SKEKA := '8'
              GOTO ABEND1
    END

/.##エラーＦＳＯＲＴ##./
PSORT:

    ?STEP :=   'PSORT   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT    INFILE-?RCVERRF,INRL-96,INBF-42,
            OUTFILE-?RCVERRF,OUTBF-42,
            KEY-1!54!CA,
            RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【エラーＤＴＳＯＲＴ】'
              GOTO ABEND
    END

/.##エラー重複チェック処理##./
SKE1000B:

    ?STEP :=   'SKE1000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-RCVERRF1,TOFILE-?RCVERRF
    OVRF      FILE-RCVERRF2,TOFILE-RCVOK.TOKBLIB
    OVRF      FILE-RCVERRF3,TOFILE-RCVNG.TOKBLIB
    CALL      PGM-SKE1000B.TOKELIB
    IF        @PGMEC    ^=   0
              THEN
              ?KEKA4 :=  '【エラーＤＴ重複チェック】'
              GOTO ABEND
    END


/.##エラー重複チェック後戻し##./
PERRFILE:

    ?STEP :=   'PERRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-RCVOK.TOKBLIB,TOFILE-?RCVERRF,ADD-@NO,BF-42
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【前回エラーＣＯＰＹ】'
              GOTO ABEND1
    END
RTN: /.##ﾌﾟﾛｸﾞﾗﾑ正常時、終了ﾒｯｾｰｼﾞ##./

    OVRF      FILE-KANKENL1,TOFILE-KENKANL1.TOKKLIB
    CALL      PGM-SKE0811B.TOKELIB,PARA-(?P7,?PDATE,?PTIME,'0','0')

    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '更新結果等を確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
/.##資源開放##./
    RELEASE FILE-RUISYUF.TOKKLIB!@XCL/   /.##出荷検品情報ﾌｧｲﾙ##./
                 RUIKONF.TOKKLIB!@XCL    /.##出荷検品梱包ﾌｧｲﾙ##./

    RETURN    PGMEC-@PGMEC

ABEND:/.##ﾌﾟﾛｸﾞﾗﾑ異常終了時、終了ﾒｯｾｰｼﾞ##./

    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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
/.##資源開放##./
    RELEASE FILE-RUISYUF.TOKKLIB!@XCL/   /.##出荷検品情報ﾌｧｲﾙ##./
                 RUIKONF.TOKKLIB!@XCL    /.##出荷検品梱包ﾌｧｲﾙ##./

    RETURN    PGMEC-@PGMEC

ABEND1:/.##ﾌﾟﾛｸﾞﾗﾑ異常終了時、終了ﾒｯｾｰｼﾞ##./

    OVRF      FILE-KANKENL1,TOFILE-KENKANL1.TOKKLIB
    CALL      PGM-SKE0811B.TOKELIB,PARA-(?P7,?PDATE,?PTIME,'1',?SKEKA)
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    ?KEKA4 :=  '（更新処理中）'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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
/.##資源開放##./
    RELEASE FILE-RUISYUF.TOKKLIB!@XCL/   /.##出荷検品情報ﾌｧｲﾙ##./
                 RUIKONF.TOKKLIB!@XCL    /.##出荷検品梱包ﾌｧｲﾙ##./

    RETURN    PGMEC-@PGMEC

```
