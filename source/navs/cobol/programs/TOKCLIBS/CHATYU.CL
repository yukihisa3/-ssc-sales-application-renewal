/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :  受配信管理システム                     *  ./
/. *   JOB-ID      :  CHATYU                                 *  ./
/. *   JOB-NAME    :  受信待受処理・発注系                   *  ./
/. ***********************************************************  ./
    PGM  (P1-?PARAFIL)
/.##振分ＪＯＢ引渡パラメータ##./
    PARA ?PARAFIL ,STRING*8,IN,VALUE-'        '  /.パラファイル名./
/.##パラメタ定義##./
    VAR  ?JDATE   ,STRING*8,VALUE-'00000000' /.受信日./
    VAR  ?JTIME   ,STRING*4,VALUE-'0000'     /.受信時間./
    VAR  ?DTKBN   ,STRING*2,VALUE-'  '       /.データ区分./
    VAR  ?JHSNKBN ,STRING*1,VALUE-' '        /.受配信区分./
    VAR  ?TORICD  ,STRING*8,VALUE-'00000000' /.取引先CD./
    VAR  ?JKAISEN ,STRING*1,VALUE-' '        /.受信回線./
    VAR  ?JTEJUN  ,STRING*1,VALUE-' '        /.受信手順./
    VAR  ?KTANNM  ,STRING*15,VALUE-'               ' /.固定端末名./
    VAR  ?EDINO   ,STRING*3,VALUE-'000'      /.ＥＤＩ相手番号./
    VAR  ?EDIFILNO,STRING*2,VALUE-'00'       /.ＥＤＩファイル番号./
    VAR  ?RHENKBN ,STRING*1,VALUE-' '        /.レイアウト変換区分./
    VAR  ?LEN     ,STRING*4,VALUE-'0000'     /.レコード長./
    VAR  ?DTHEPG  ,STRING*8,VALUE-'        ' /.データ変換ＰＧ名./
    VAR  ?JHFNM   ,STRING*8,VALUE-'        ' /.受配信ファイル名./
    VAR  ?KJOB    ,STRING*8,VALUE-'        ' /.起動ＪＯＢ名./
    VAR  ?KAISHU  ,STRING*1,VALUE-' '        /.回線種別./
    VAR  ?KSNO    ,STRING*1,VALUE-'0'        /.回線制御番号./
    VAR  ?TGFNM   ,STRING*8,VALUE-'        ' /.トリガーファイル名./
    VAR  ?ASFNM   ,STRING*8,VALUE-'        ' /.アンサーファイル名./
    VAR  ?PMFNM   ,STRING*8,VALUE-'        ' /.パラメータファイル名./
    VAR  ?SYKEKA  ,STRING*1,VALUE-' '        /.処理結果./

    VAR  ?KEKA    ,STRING*4,VALUE-'0000'     /.結果コード./
    VAR  ?ANSSTS  ,STRING*4,VALUE-'    '     /.受信結果./
    VAR  ?HENKEKA ,STRING*4,VALUE-'    '     /.変換処理結果./

    VAR  ?JYSNTMED,STRING*04,VALUE-'0000'    /. 受信終了時刻        ./
    VAR  ?DTCVTMST,STRING*04,VALUE-'0000'    /. データ変換開始時刻  ./
    VAR  ?DTCVTMED,STRING*04,VALUE-'0000'    /. データ変換終了時刻  ./
    VAR  ?JSNFLG  ,STRING*01,VALUE-'0'       /. 受信状況ＦＬＧ      ./
    VAR  ?HENFLG  ,STRING*01,VALUE-'0'       /. 変換状況ＦＬＧ      ./
    VAR  ?UPDFLG  ,STRING*01,VALUE-'0'       /. 更新状況ＦＬＧ      ./
    VAR  ?FINFLG  ,STRING*01,VALUE-'0'       /. 実行完了ＦＬＧ      ./

/.##ワーク定義##./
    VAR  ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR  ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR  ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR  ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR  ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR  ?PGMID    ,STRING*8,VALUE-'CHATYU '   /.PROGRAM-ID./
    VAR  ?STEP     ,STRING*8                   /.STEP-ID./
    VAR  ?LIBNAME  ,STRING*8,VALUE-'TOKELIB '  /.ﾌﾟﾛｸﾞﾗﾑ実行LIB./
    VAR  ?LIBN     ,NAME                       /.ﾌﾟﾛｸﾞﾗﾑ名編集用./
    VAR  ?PGN      ,NAME                       /.ﾌﾟﾛｸﾞﾗﾑ名編集用./
    VAR  ?PGLIB    ,NAME!MOD                   /.ﾌﾟﾛｸﾞﾗﾑ名編集用./
    VAR  ?PGNAME   ,STRING*17                  /.ﾌﾟﾛｸﾞﾗﾑ名表示用./
    VAR  ?HTIME    ,STRING*8,VALUE-'        '  /.ｼｽﾃﾑ時間退避用./
/.##ﾌｧｲﾙ変換ﾜｰｸ##./
    VAR  ?LIBN1    ,NAME                       /.ﾗｲﾌﾞﾗﾘ名前型./
    VAR  ?FILN     ,NAME                       /.ﾌｧｲﾙ名前型./
    VAR  ?FILID    ,STRING*17                  /.ﾌｧｲﾙ名表示用./
/.##ワーク定義２##./
    VAR  ?LIBNM1  ,STRING*08,VALUE-'TOKJLIB'   /.ﾗｲﾌﾞﾗﾘ名１./
    VAR  ?LIBNM2  ,STRING*08,VALUE-'ONLBLIB'   /.ﾗｲﾌﾞﾗﾘ名２./
    VAR  ?LIBNM3  ,STRING*08,VALUE-'TOKCLIBO'  /.ﾗｲﾌﾞﾗﾘ名３./
    VAR  ?LIBNM4  ,NAME                        /.ﾗｲﾌﾞﾗﾘ名４./
    VAR  ?LIBNM   ,NAME                        /.ﾗｲﾌﾞﾗﾘ名./
    VAR  ?JFILNM  ,NAME!MOD                    /.受信ﾌｧｲﾙ名./
    VAR  ?PARAF   ,NAME!MOD                    /.ﾊﾟﾗﾒｰﾀﾌｧｲﾙ名./
    VAR  ?ANSERF  ,NAME!MOD                    /.ｱﾝｻｰﾌｧｲﾙ名./
    VAR  ?TIMES   ,STRING*04,VALUE-'0000'      /.開始時間./
    VAR  ?TIMEE   ,STRING*04,VALUE-'0000'      /.終了時間./
    VAR  ?PGNM    ,NAME                        /.変換ＰＧ名./
    VAR  ?HENPG   ,NAME                        /.ﾃﾞｰﾀ変換ＰＧ名./
    VAR  ?PGMLIB  ,NAME!MOD                    /.変換ＰＧ./
    VAR  ?JSNTIME ,STRING*04,VALUE-'0000'      /.受信終了時間./
    VAR  ?HTIMES  ,STRING*04,VALUE-'0000'      /.変換開始時間./
    VAR  ?HTIMEE  ,STRING*04,VALUE-'0000'      /.変換終了時間./
    VAR  ?UPTIMES ,STRING*04,VALUE-'0000'      /.更新開始時間./
    VAR  ?UPTIMEE ,STRING*04,VALUE-'0000'      /.更新終了時間./
    VAR  ?JIKAN   ,STRING*04,VALUE-'0000'      /.時間./
    VAR  ?BKFIL   ,NAME                        /.ﾊﾞｯｸｱｯﾌﾟﾌｧｲﾙ名./
    VAR  ?KIDOJB  ,NAME                        /.起動ＪＯＢ名./
    VAR  ?FGLOOP  ,STRING*1                    /.ループ判定./
    /.エラーメール発行用ワーク./
    VAR ?ERKBN    ,STRING*03,VALUE-'   '       /.区分./
    VAR ?ERDATE   ,STRING*08,VALUE-'00000000'  /.バッチ日付./
    VAR ?ERTIME   ,STRING*04,VALUE-'0000'      /.バッチ時刻./
    VAR ?ERTKCD   ,STRING*08,VALUE-'00000000'  /.バッチ取引先./

    ACTMSGQ MAXRMSG-50

/.##プログラム開始メッセージ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
    SNDMSG MSG-'＊　受信待受処理・発注系　開始　＊',TO-XCTL
    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL

/.##パラメータファイル名編集##./
    ?FILN   := %NAME(?PARAFIL)
    ?LIBNM  := %NAME(?LIBNM1)
    ?PARAF  := %NCAT(?FILN,?LIBNM)
    ?MSGX   := '***   PARAFIL =' && ?PARAFIL && '.' && ?LIBNM1
    SNDMSG  ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKJLIB/TOKDTLIB

/.##パラメタファイルより受信パラメタ取得##./
SNJ0540B:
    CALL SCVMSG.TOKCLIBO,PARA-('ﾊﾟﾗﾒﾀ ｼｭﾄｸ     START')
    ?STEP := 'SNJ0540B'
    ?MSGX := '***   '  && ?STEP   &&   '        ***'
    SNDMSG   ?MSGX,TO-XCTL

    OVRF FILE-PARAFIL,TOFILE-?PARAF
/. パラメータ ./
/.   出力                     ./
/.     1.受信日               ./
/.     2.受信時間             ./
/.     3.データ区分           ./
/.     4.受配信区分           ./
/.     5.取引先コード         ./
/.     6.受信回線             ./
/.     7.受信手順             ./
/.     8.固定端末名           ./
/.     9.レコード長           ./
/.    10.データ変換ＰＧ名     ./
/.    11.受配信ファイル名     ./
/.    12.起動ＪＯＢ名         ./
/.    13.回線種別             ./
/.    14.回線制御番号         ./
/.    15.トリガーファイル名   ./
/.    16.アンサーファイル名   ./
/.    17.パラメータファイル名 ./
/.    18.処理結果             ./

    CALL PGM-SNJ0540B.TOKELIBO,
         PARA-(?JDATE,?JTIME,?DTKBN,?JHSNKBN,?TORICD,
               ?JKAISEN,?JTEJUN,?KTANNM,?LEN,?DTHEPG,?JHFNM,
               ?KJOB,?KAISHU,?KSNO,?TGFNM,?ASFNM,
               ?PMFNM,?SYKEKA)
    ?PGMEC  := @PGMEC
    ?PGMEM  := @PGMEM
    IF  ?PGMEC ^= 0 THEN  /.PG実行異常./
        ?KEKA  := 'K515'
    ELSE
        IF  ?SYKEKA = '9' THEN  /.パラメータファイルなし./
            ?KEKA  := 'K515'
        END
    END


 /. ?MSGX := '## TST JDATE=' && ?JDATE
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST JTIME=' && ?JTIME
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST DTKBN=' && ?DTKBN
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST JHSNKBN=' && ?JHSNKBN
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST TORICD=' && ?TORICD
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST JKAISEN=' && ?JKAISEN
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST JTEJUN=' && ?JTEJUN
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST KTANNM=' && ?KTANNM
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST LEN=' && ?LEN
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST DTHEPG=' && ?DTHEPG
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST JHFNM=' && ?JHFNM
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST KJOB=' && ?KJOB
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST KAISHU=' && ?KAISHU
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST KSNO=' && ?KSNO
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST TGFNM=' && ?TGFNM
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST ASFNM=' && ?ASFNM
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST PMFNM=' && ?PMFNM
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST SYKEKA=' && ?SYKEKA
    SNDMSG  ?MSGX,TO-XCTL      ./

/.##ｴﾗｰ判断##./
ERRCHK:
    CALL SCVMSG.TOKCLIBO,PARA-('ﾊﾟﾗﾒﾀ ｼｭﾄｸ CHK START')
    ?STEP := 'ERRCHK  '
    ?MSGX := '***   '  && ?STEP   &&   '        ***'
    SNDMSG  ?MSGX,TO-XCTL

    ?MSGX := '## 結果 = ' && ?KEKA
    SNDMSG  ?MSGX,TO-XCTL

    IF  ?KEKA ^= '0000'  THEN  /.PG実行異常./
        GOTO  ABEND
    END

/.## 受配信ファイル名編集 ##./
    ?FILN   := %NAME(?JHFNM)
    ?LIBNM  := %NAME(?LIBNM2)
    ?JFILNM := %NCAT(?FILN,?LIBNM)
    ?MSGX   := '***   JHFNM   =' && ?JHFNM && '.' && ?LIBNM2
    SNDMSG  ?MSGX,TO-XCTL

/.## アンサーファイル名編集 ##./
    ?FILN   := %NAME(?ASFNM)
    ?LIBNM  := %NAME(?LIBNM1)
    ?ANSERF := %NCAT(?FILN,?LIBNM)
    ?MSGX   := '***   ASFNM   =' && ?ASFNM && '.' && ?LIBNM1
    SNDMSG  ?MSGX,TO-XCTL

/.## 変換ＰＧ名編集 ##./
    ?FILN   := %NAME(?DTHEPG)
    ?LIBNM  := %NAME(?LIBNM3)
    ?PGMLIB := %NCAT(?FILN,?LIBNM)
    ?MSGX   := '***   DTHEPG  =' && ?DTHEPG && '.' && ?LIBNM3
    SNDMSG  ?MSGX,TO-XCTL

/.##事前受信ファイル初期化 2013/01/10 NAV 追加##./
    CALL SCVMSG.TOKCLIBO,PARA-('ｶｺﾌﾞﾝｼﾞｭｼﾝﾌｧｲﾙ ｸﾘｱ  ')
    CLRFILE  ?JFILNM
    IF ?PGMEC ^= 0 THEN
       ?KEKA := 'K557'
       GOTO  ABEND
    END


/.##受信判定##./
SNJ0560B:
    CALL SCVMSG.TOKCLIBO,PARA-('ｼﾞｭｼﾝ ﾊﾝﾃｲ     START')
    ?FGLOOP  := '0'

    WHILE  ?FGLOOP = '0'
      DO
        CALL WAITPG.TOKELIB,PARA-('000030')  /. 30秒待ち ./

        ?STEP := 'SNJ0560B'
        ?MSGX := '***   '  && ?STEP   &&   '        ***'
        SNDMSG   ?MSGX,TO-XCTL

        OVRF FILE-ANSER,TOFILE-?ANSERF

/. パラメータ ./
/.   出力                         ./
/.     1.アンサーＦ終了ステータス ./
/.     2.受信判定                 ./

        CALL PGM-SNJ0560B.TOKELIBO,
             PARA-(?ANSSTS,?SYKEKA)

        ?PGMEC := @PGMEC
        ?PGMEM := @PGMEM
        IF  ?PGMEC ^= 0 THEN  /.PG実行異常./
            ?JSNFLG := '9'
            ?KEKA   := 'K100'
            ?FGLOOP := 'E'
            BREAK
        END

    /.  ?MSGX := '## TST ANSSTS=' && ?ANSSTS
        SNDMSG  ?MSGX,TO-XCTL
        ?MSGX := '## TST SYKEKA=' && ?SYKEKA
        SNDMSG  ?MSGX,TO-XCTL  ./

        IF  ?SYKEKA = ' ' THEN  /.受信済み./
            CASE  ?ANSSTS OF
              # '0000' #  /.正常受信./
                ?JSNFLG := '1'

              # '0100' #  /.正常受信./
                ?JSNFLG := '1'

              # '00  ' #  /.正常受信./
                ?JSNFLG := '1'

              # '0088' #  /.データゼロ件./
                ?JSNFLG := '1'
                ?KEKA   := ?ANSSTS

              # '0117' #  /.データゼロ件./
                ?JSNFLG := '1'
                ?KEKA   := ?ANSSTS

              # '03  ' #  /.データゼロ件./
                ?JSNFLG := '1'
                ?KEKA   := ?ANSSTS

              # '0062' #  /.相手話中./
                ?JSNFLG := '9'
                ?KEKA   := ?ANSSTS

              # '62  ' #  /.相手話中./
                ?JSNFLG := '9'
                ?KEKA   := ?ANSSTS

              ELSE        /.異常受信./
                ?JSNFLG := '9'
                ?KEKA   := ?ANSSTS
            END

            ?FGLOOP := 'E'
            BREAK
        END
    END


/.## 受信終了時間取得 ##./
    ?JIKAN   := @STIME
    ?JSNTIME := %SBSTR(?JIKAN,1,4)

/.##ｴﾗｰ判断##./
ERRCHK2:
    CALL SCVMSG.TOKCLIBO,PARA-('ｼﾞｭｼﾝ ﾊﾝﾃｲ CHK START')
    ?STEP := 'ERRCHK2 '
    ?MSGX := '***   '  && ?STEP   &&   '        ***'
    SNDMSG  ?MSGX,TO-XCTL

    ?MSGX := '## 結果 = ' && ?KEKA
    SNDMSG  ?MSGX,TO-XCTL

    IF  ?KEKA ^= '0000'  THEN
        CASE  ?KEKA OF
          # '03  ' #   /. データゼロ件　./
            GOTO  RTN

          # '0088' #   /. データゼロ件  ./
            GOTO  RTN

          # '0117' #   /. データゼロ件  ./
            GOTO  RTN

          # '0062' #   /. 相手話中 ./
            ?JSNTIME := '0000'
            GOTO  RTN

          # '62  ' #   /. 相手話中 ./
            ?JSNTIME := '0000'
            GOTO  RTN

          ELSE        /.異常受信./
            ?JSNTIME := '0000'
            GOTO  ABEND
        END
    END

/.##受信ファイルバックアップ##./
DATABACK:
    CALL SCVMSG.TOKCLIBO,PARA-('ｼﾞｭｼﾝFﾊﾞｯｸｱｯﾌﾟ START')
    ?STEP := 'DATABACK'
    ?MSGX := '***   '  && ?STEP   &&   '        ***'
    SNDMSG   ?MSGX,TO-XCTL

    ?MSGX := 'LEN = '  && ?LEN
    SNDMSG   ?MSGX,TO-XCTL

/.##受信ﾌｧｲﾙノ編集##./
    CASE  ?LEN OF
      # '0128' #
/.       OVRF FILE-CVCSG001,TOFILE-?JFILNM
         OVRF FILE-BKCVCS01,TOFILE-BACK0128.ONLBLIB
         CALL SJU2010B.TOKELIB,PARA-(?JDATE,?JTIME,?TORICD)
         SBMJOB JOB-BACK0128,JOBD-CVCS.XUCL,JOBK-@B,
             PGM-PJU20100.TOKCLIBO,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
             @S/@T,LIBL-ONLBLIB,PARA-(?JDATE,?JIKAN,?TORICD,
             ?JHFNM,?LIBNM2,?KEKA)
./       SBMJOB JOB-BACK0128,JOBD-NEWONL1.XUCL,JOBK-@B,
         PGM-PJU20100.TOKCLIBO,LOG-@YES!1024,
         PTY-5,PGMEL-@I/@L/@S/@T,
         LIBL-TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/ONLBLIB,
         PARA-(?JDATE,?JIKAN,?TORICD,?JHFNM,?LIBNM2)
/.       PARA-(?JDATE,?JIKAN,?TORICD,?JHFNM,?LIBNM2,?KEKA) ./
         ?PGMEC := @PGMEC
         ?PGMEM := @PGMEM
         IF  ?PGMEC ^= 0 THEN
             ?KEKA  :=  'K101'
         END
      # '0256' #
/.       OVRF FILE-CVCSG002,TOFILE-?JFILNM
         OVRF FILE-BKCVCS02,TOFILE-BACK0256.ONLBLIB
         CALL SJU2020B.TOKELIB,PARA-(?JDATE,?JTIME,?TORICD)
         SBMJOB JOB-BACK0256,JOBD-CVCS.XUCL,JOBK-@B,
             PGM-PJU20200.TOKCLIBO,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
             @S/@T,LIBL-ONLBLIB,PARA-(?JDATE,?JIKAN,?TORICD,
             ?JHFNM,?LIBNM2,?KEKA)
./       SBMJOB JOB-BACK0256,JOBD-NEWONL1.XUCL,JOBK-@B,
         PGM-PJU20200.TOKCLIBO,LOG-@YES!1024,
         PTY-5,PGMEL-@I/@L/@S/@T,
         LIBL-TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/ONLBLIB,
         PARA-(?JDATE,?JIKAN,?TORICD,?JHFNM,?LIBNM2)
/.       PARA-(?JDATE,?JIKAN,?TORICD,?JHFNM,?LIBNM2,?KEKA)   ./
         ?PGMEC := @PGMEC
         ?PGMEM := @PGMEM
         IF  ?PGMEC ^= 0 THEN
             ?KEKA  :=  'K101'
         END
      # '0264' #
/.       OVRF FILE-CVCSG003,TOFILE-?JFILNM
         OVRF FILE-BKCVCS03,TOFILE-BACK0264.ONLBLIB
         CALL SJU2040B.TOKELIB,PARA-(?JDATE,?JTIME,?TORICD)
         SBMJOB JOB-BACK0264,JOBD-CVCS.XUCL,JOBK-@B,
             PGM-PJU20300.TOKCLIBO,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
             @S/@T,LIBL-ONLBLIB,PARA-(?JDATE,?JIKAN,?TORICD,
             ?JHFNM,?LIBNM2,?KEKA)
./       SBMJOB JOB-BACK0264,JOBD-NEWONL1.XUCL,JOBK-@B,
         PGM-PJU20300.TOKCLIBO,LOG-@YES!1024,
         PTY-5,PGMEL-@I/@L/@S/@T,
         LIBL-TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/ONLBLIB,
         PARA-(?JDATE,?JIKAN,?TORICD,?JHFNM,?LIBNM2)
/.       PARA-(?JDATE,?JIKAN,?TORICD,?JHFNM,?LIBNM2,?KEKA) ./
         ?PGMEC := @PGMEC
         ?PGMEM := @PGMEM
         IF  ?PGMEC ^= 0 THEN
             ?KEKA  :=  'K101'
         END
      # '2048' #
/.       OVRF FILE-CVCSG003,TOFILE-?JFILNM
         OVRF FILE-BKCVCS03,TOFILE-BACK2048.ONLBLIB
         CALL SJU2030B.TOKELIB,PARA-(?JDATE,?JTIME,?TORICD)
         SBMJOB JOB-BACK2048,JOBD-CVCS.XUCL,JOBK-@B,
             PGM-PJU20400.TOKCLIBO,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
             @S/@T,LIBL-ONLBLIB,PARA-(?JDATE,?JIKAN,?TORICD,
             ?JHFNM,?LIBNM2,?KEKA)
./       SBMJOB JOB-BACK2048,JOBD-NEWONL1.XUCL,JOBK-@B,
         PGM-PJU20400.TOKCLIBO,LOG-@YES!1024,
         PTY-5,PGMEL-@I/@L/@S/@T,
         LIBL-TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/ONLBLIB,
         PARA-(?JDATE,?JIKAN,?TORICD,?JHFNM,?LIBNM2)
/.       PARA-(?JDATE,?JIKAN,?TORICD,?JHFNM,?LIBNM2,?KEKA) ./
         ?PGMEC := @PGMEC
         ?PGMEM := @PGMEM
         IF  ?PGMEC ^= 0 THEN
             ?KEKA  :=  'K101'
         END
      # '0384' #
         SBMJOB JOB-BACK0384,JOBD-NEWONL1.XUCL,JOBK-@B,
         PGM-PJU20600.TOKCLIBO,LOG-@YES!1024,
         PTY-5,PGMEL-@I/@L/@S/@T,
         LIBL-TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/ONLBLIB,
         PARA-(?JDATE,?JIKAN,?TORICD,?JHFNM,?LIBNM2)
         ?PGMEC := @PGMEC
         ?PGMEM := @PGMEM
         IF  ?PGMEC ^= 0 THEN
             ?KEKA  :=  'K101'
         END
      ELSE
         ?KEKA := 'K101'
    END

/.##ｴﾗｰ判断##./
ERRCHK3:
    CALL SCVMSG.TOKCLIBO,PARA-('ｼﾞｭｼﾝF BKUPCHK START')
    ?STEP := 'ERRCHK3 '
    ?MSGX := '***   '  && ?STEP   &&   '        ***'
    SNDMSG  ?MSGX,TO-XCTL

    ?MSGX := '## 結果 = ' && ?KEKA
    SNDMSG  ?MSGX,TO-XCTL
    IF  ?KEKA ^= '0000'  THEN
        GOTO  ABEND
    END

    ?MSGX := '### 変換開始'
    SNDMSG   ?MSGX,TO-XCTL
    ?MSGX := '### 取引先 = ' && ?TORICD
    SNDMSG   ?MSGX,TO-XCTL

/.##受信ﾃﾞｰﾀ変換##./
HENKAN:
    CALL SCVMSG.TOKCLIBO,PARA-('ｼﾞｭｼﾝDT ﾍﾝｶﾝ   START')
    ?STEP := 'HENKAN  '
    ?MSGX := '***   '  && ?STEP   &&   '        ***'
    SNDMSG   ?MSGX,TO-XCTL

   /.##変換開始時間取得##./
    ?HTIME   := @STIME
    ?HTIMES  := %SBSTR(?HTIME,1,4)
    ?HENKEKA := '0000'

/. パラメータ ./
/.   入力                          ./
/.     1.受信日                    ./
/.     2.受信時間                  ./
/.     3.取引先コード              ./
/.     4.回線種別                  ./
/.     5.回線制御番号              ./
/.     6.受配信ファイルライブラリ  ./
/.     7.受配信ファイル名          ./
/.   出力                          ./
/.     1.変換結果コード            ./

/.  ?MSGX := '## TST IN JDATE=' && ?JDATE
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST IN JTIME=' && ?JTIME
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST IN TORICD=' && ?TORICD
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST IN KAISHU=' && ?KAISHU
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST IN KSNO=' && ?KSNO
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST IN LIBNM2=' && ?LIBNM2
    SNDMSG  ?MSGX,TO-XCTL
    ?MSGX := '## TST IN JHFNM=' && ?JHFNM
    SNDMSG  ?MSGX,TO-XCTL ./

    CALL PGM-?PGMLIB,PARA-(?JDATE,?JTIME,?TORICD,?KAISHU,?KSNO,?LIBNM2,
                           ?JHFNM,?HENKEKA)
    CALL SCVMSG.TOKCLIBO,PARA-('PG END              ')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM

/.  ?MSGX := '## TST OT HENKEKA=' && ?HENKEKA
    SNDMSG  ?MSGX,TO-XCTL  ./

    IF  ?PGMEC ^= 0 THEN
        ?HENFLG := '9'
        IF   ?HENKEKA ^=  '    '     THEN
          IF   ?HENKEKA ^=  '0000'   THEN
            IF   ?HENKEKA = 'K525'  THEN
                 CALL SCVMSG.TOKCLIBO,PARA-('PG ABEND ST-CHK     ')
                 ?MSGX   := '## 変換異常終了 結果 = ' && ?HENKEKA
                 SNDMSG MSG-?MSGX,TO-XCTL
                 ?KEKA   :=  ?HENKEKA
            ELSE
                 ?KEKA   := 'K102'
            END
          ELSE
            ?KEKA   := 'K102'
          END
        ELSE
          ?KEKA   := 'K102'
        END
    ELSE
        ?HENFLG := '1'
        ?KEKA   :=  ?HENKEKA
    END

/.##ｴﾗｰ判断##./
ERRCHK4:
    CALL SCVMSG.TOKCLIBO,PARA-('ｼﾞｭｼﾝDT CV CHK START')
    ?STEP := 'ERRCHK4 '
    ?MSGX := '***   '  && ?STEP   &&   '        ***'
    SNDMSG  ?MSGX,TO-XCTL

    ?MSGX := '## 結果 = ' && ?KEKA
    SNDMSG  ?MSGX,TO-XCTL
/.  IF  ?KEKA ^= '0000'  THEN ./
    IF  ?KEKA  = 'K525'  THEN
        GOTO  ABEND
    END

   /.##変換終了時間取得##./
    ?HTIME  := @STIME
    ?HTIMEE := %SBSTR(?HTIME,1,4)

/.##実行ログ更新##./
DATESET:
    CALL SCVMSG.TOKCLIBO,PARA-('ｼｮﾘｹｯｶ LOG SET START')
    ?STEP := 'DATESET '
    ?MSGX := '***   '  && ?STEP   &&   '        ***'
    SNDMSG   ?MSGX,TO-XCTL

    OVRF  FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB

/. パラメータ ./
/.   入力                    ./
/.     1.日付                ./
/.     2.時刻                ./
/.     3.取引先コード        ./
/.     4.受信終了時刻        ./
/.     5.データ変換開始時刻  ./
/.     6.データ変換終了時刻  ./
/.     7.受信状況ＦＬＧ      ./
/.     8.変換状況ＦＬＧ      ./
/.     9.処理結果コード      ./
    CALL    PGM-SNJ0710B.TOKELIBO,PARA-(?JDATE,?JTIME,?TORICD,
            ?JSNTIME,?HTIMES,?HTIMEE,?JSNFLG,?HENFLG,?KEKA)
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF ?PGMEC ^= 0 THEN
       ?KEKA := 'K103'
       GOTO  ABEND
    END

/.##振分け件数ﾘｽﾄ出力##./
KENLSTH:
    CALL SCVMSG.TOKCLIBO,PARA-('ｹﾝｽｳ LST STA KENLSTH')
    ?STEP := 'KENLSTH '
    ?MSGX := '***   '  && ?STEP   &&   '        ***'
    SNDMSG   ?MSGX,TO-XCTL

/. パラメータ ./
/.   入力                          ./
/.     1.受信日                    ./
/.     2.受信時間                  ./
/.     3.取引先コード              ./
/.     4.回線種別                  ./
/.     5.回線制御番号              ./
/.  SBMJOB JOB-KENLST,JOBD-CVCS.XUCL,JOBK-@B,PGM-KENLST.TOKELIB, ./
    SBMJOB JOB-KENLSTH,JOBD-CVCS.XUCL,JOBK-@B,PGM-KENLST.TOKCLIBO,
           VSIZE-5,RSIZE-16,LOG-@YES!1024,PARA-(?JDATE,?JTIME,
           ?TORICD,?KAISHU,?KSNO)

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:
/. 受信ファイルクリア ./
    CALL SCVMSG.TOKCLIBO,PARA-('ﾊｯﾁｭｳｼﾞｭｼﾝﾌｧｲﾙ ｸﾘｱ  ')
    CLRFILE  ?JFILNM
    IF ?PGMEC ^= 0 THEN
       ?KEKA := 'K545'
       GOTO  ABEND
    END

/.##結果ﾘｽﾄ発行##./
    CALL SCVMSG.TOKCLIBO,PARA-('ｹｯｶ LST START (RTN) ')
/. パラメータ ./
/.   入力                          ./
/.     1.受信日                    ./
/.     2.受信時間                  ./
/.     3.取引先コード              ./
/.     4.結果コード                ./
    SBMJOB JOB-KEKALSH,JOBD-CVCS.XUCL,JOBK-@B,
           PGM-SNJ0590L.TOKELIBO,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
           @S/@T,LIBL-TOKFLIB/TOKELIB/TOKJLIB/TOKELIBO,
           PARA-(?JDATE,?JTIME,?TORICD,?KEKA)

    CALL SCVMSG.TOKCLIBO,PARA-('ｾｲｼﾞｮｳ ｼｭｳﾘｮｳ CHATYU')

/.##回線管理マスタ　使用フラグ、使用取引先クリア ./
    CALL SCVMSG.TOKCLIBO,PARA-('ｶｲｾﾝ ｶﾝﾘﾏｽﾀ ﾌﾗｸﾞｼｮｷｶ')

    OVRF  FILE-JSMKAIL1,TOFILE-JSMKAIL1.TOKJLIB
    CALL SNJ9080B.TOKELIBO,PARA-(?KAISHU,?KSNO)

/.##当日スケジュールマスタ、実行完了フラグ更新./
    CALL SCVMSG.TOKCLIBO,PARA-('ﾄｳｼﾞﾂｽｹｼﾞｭｰﾙM ｺｳｼﾝ  ')

/.処理結果設定（K523:受信完了を設定)   ./
/.  ?KEKA  :=  'K523' ./

    OVRF  FILE-JSMKAIL1,TOFILE-JSMKAIL1.TOKJLIB
    CALL SNJ9110B.TOKELIBO,PARA-(?JDATE,?JTIME,
                                    ?TORICD,?SYKEKA,?KEKA)


    ?MSGX := '***   ' && ?PGMID && ' END    ***'
    SNDMSG ?MSGX,TO-XCTL
    RETURN PGMEC-@PGMEC


/.##ﾌﾟﾛｸﾞﾗﾑ正常終了ゼロ件／リダイアルエラー##./
RTN1:
/.##実行ログ更新##./

    CALL SCVMSG.TOKCLIBO,PARA-('ｹｯｶ LST START (RTN1)')
    DEFLIBL TOKELIB/TOKFLIB/TOKJLIB/TOKDTLIB
    OVRF  FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB

/. パラメータ ./
/.   入力                    ./
/.     1.日付                ./
/.     2.時刻                ./
/.     3.取引先コード        ./
/.     4.受信終了時刻        ./
/.     5.データ変換開始時刻  ./
/.     6.データ変換終了時刻  ./
/.     7.受信状況ＦＬＧ      ./
/.     8.変換状況ＦＬＧ      ./
/.     9.処理結果コード      ./
    CALL    PGM-SNJ0710B.TOKELIBO,PARA-(?JDATE,?JTIME,?TORICD,
            ?JSNTIME,?HTIMES,?HTIMEE,?JSNFLG,?HENFLG,?KEKA)

    CALL SCVMSG.TOKCLIBO,PARA-('0ｹﾝ/ﾘﾀﾞｲﾔﾙEND HENTEL')

/.##回線管理マスタ　使用フラグ、使用取引先クリア ./
    CALL SCVMSG.TOKCLIBO,PARA-('ｶｲｾﾝ ｶﾝﾘﾏｽﾀ ﾌﾗｸﾞｼｮｷｶ')

    OVRF  FILE-JSMKAIL1,TOFILE-JSMKAIL1.TOKJLIB
    CALL SNJ9080B.TOKELIBO,PARA-(?KAISHU,?KSNO)


    ?MSGX := '***   ' && ?PGMID && ' END    ***'
    SNDMSG ?MSGX,TO-XCTL
    RETURN PGMEC-@PGMEC


/.##ﾌﾟﾛｸﾞﾗﾑ異常終了##./
ABEND:

  /.## エラーメール用データ出力　セット 開始  ##./
    ?ERKBN   :=  '001'
    ?ERDATE  :=  ?JDATE
    ?ERTIME  :=  ?JTIME
    ?ERTKCD  :=  ?TORICD
    CALL ERR0010B.TOKSOLIB,PARA-(?ERKBN,?ERDATE,?ERTIME,?ERTKCD)
  /.## エラーメール用データ出力　セット 終了  ##./
/.##実行ログ更新##./
    CALL SCVMSG.TOKCLIBO,PARA-('ｼｮﾘｹｯｶ LOG SET START')
    DEFLIBL TOKELIB/TOKFLIB/TOKJLIB/TOKDTLIB
    OVRF  FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB

/. パラメータ ./
/.   入力                    ./
/.     1.日付                ./
/.     2.時刻                ./
/.     3.取引先コード        ./
/.     4.受信終了時刻        ./
/.     5.データ変換開始時刻  ./
/.     6.データ変換終了時刻  ./
/.     7.受信状況ＦＬＧ      ./
/.     8.変換状況ＦＬＧ      ./
/.     9.処理結果コード      ./
    CALL    PGM-SNJ0710B.TOKELIBO,PARA-(?JDATE,?JTIME,?TORICD,
            ?JSNTIME,?HTIMES,?HTIMEE,?JSNFLG,?HENFLG,?KEKA)

/.##結果ﾘｽﾄ発行##./
    CALL SCVMSG.TOKCLIBO,PARA-('ｹｯｶ LST        START')
/. パラメータ ./
/.   入力                          ./
/.     1.受信日                    ./
/.     2.受信時間                  ./
/.     3.取引先コード              ./
/.     4.結果コード                ./
    SBMJOB JOB-KEKALSH,JOBD-CVCS.XUCL,JOBK-@B,
           PGM-SNJ0590L.TOKELIBO,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
           @S/@T,LIBL-TOKFLIB/TOKELIB/TOKJLIB/TOKELIBO,
           PARA-(?JDATE,?JTIME,?TORICD,?KEKA)

    CALL SCVMSG.TOKCLIBO,PARA-('ｲｼﾞｮｳ ｼｭｳﾘｮｳ  CHATYU')

/.##回線管理マスタ　使用フラグ、使用取引先クリア ./
    CALL SCVMSG.TOKCLIBO,PARA-('ｶｲｾﾝ ｶﾝﾘﾏｽﾀ ﾌﾗｸﾞｼｮｷｶ')

    OVRF  FILE-JSMKAIL1,TOFILE-JSMKAIL1.TOKJLIB
    CALL SNJ9080B.TOKELIBO,PARA-(?KAISHU,?KSNO)


    ?PGMECX := %STRING(?PGMEC)
    ?MSG(1) := '### ' && ?PGMID && ' ABEND ' && ' ###'
    ?MSG(2) := '### PGMEC = ' &&
               %SBSTR(?PGMECX,8,4) && '   ###'
    ?MSG(3) := '### LINE  = ' && %LAST(LINE) && '   ###'
    FOR ?I := 1 TO 3
      DO ?MSGX := ?MSG(?I)
         SNDMSG ?MSGX,TO-XCTL
    END

    SNDMSG ?PGMEM,TO-XCTL
    DSPLOG OUTPUT-@LIST
    SNDMSG MSG-'＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃',
           TO-XCTL
    SNDMSG MSG-'＃＜受信処理において異常終了が発生　＞＃',
           TO-XCTL
    SNDMSG MSG-'＃　エラー情報を回収し、至急、　　　　＃',
           TO-XCTL
    SNDMSG MSG-'＃　ナブ・アシストまで連絡して下さい。＃',
           TO-XCTL
    SNDMSG MSG-'＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃',
           TO-XCTL
    RETURN PGMEC-@PGMEC
