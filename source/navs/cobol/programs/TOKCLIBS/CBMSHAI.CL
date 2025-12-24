/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :  受配信管理システム                     *  ./
/. *   JOB-ID      :  CBMSHAI                                *  ./
/. *   JOB-NAME    :  流通ＢＭＳ配信待受処理                 *  ./
/. *   CHG-DATE    :  2013/02/12                             *  ./
/. *               :  バローデータ正常送信後にデータクリア　 *  ./
/. *   CHG-DATE    :  2013/03/22                             *  ./
/. *               :  バロー植物追加　　　　　　　　　　　　 *  ./
/. *   CHG-DATE    :  2018/03/13                             *  ./
/. *               :  セキチュー追加　　　　　　　　　　　　 *  ./
/. *   CHG-DATE    :  2018/05/18                             *  ./
/. *               :  ムサシ追加　　　　　　　　　　　　　　 *  ./
/. *   CHG-DATE    :  2018/09/10                             *  ./
/. *               :  山新追加　　　　　　　　　　　　　　　 *  ./
/. *   CHG-DATE    :  2019/04/19                             *  ./
/. *               :  ジュンテンドー追加　　　　　　　　　　 *  ./
/. *   CHG-DATE    :  2020/07/29                             *  ./
/. *               :  島忠追加　　　　　　　　　　　　　　　 *  ./
/. *   CHG-DATE    :  2022/01/06                             *  ./
/. *               :  ジョイフルエーケー追加　　　　　　　　 *  ./
/. *   CHG-DATE    :  2023/01/20                             *  ./
/. *               :  アレンザＨＤ対応（３社）　　　　　　　 *  ./
/. *   CHG-DATE    :  2023/12/12                             *  ./
/. *               :  ハンズマン追加　　　　　　　　　　　　 *  ./
/. *   CHG-DATE    :  2024/11/20                             *  ./
/. *               :  ＨＩひろせ追加　　　　　　　　　　　　 *  ./
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
    VAR  ?HENKEKA ,STRING*2,VALUE-'  '       /.変換処理結果./

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
    VAR  ?PGMID    ,STRING*8,VALUE-'CBMSHAI '   /.PROGRAM-ID./
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
    VAR  ?LIBNM3  ,STRING*08,VALUE-'TOKELIBO'  /.ﾗｲﾌﾞﾗﾘ名３./
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

    ACTMSGQ MAXRMSG-50

/.##プログラム開始メッセージ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
    SNDMSG MSG-'＊　配信待受処理　開始　＊',TO-XCTL
    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL

/.##パラメータファイル名編集##./
    ?FILN   := %NAME(?PARAFIL)
    ?LIBNM  := %NAME(?LIBNM1)
    ?PARAF  := %NCAT(?FILN,?LIBNM)
    ?MSGX   := '***   PARAFIL =' && ?PARAFIL && '.' && ?LIBNM1
    SNDMSG  ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKJLIB/TOKELIBO

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

/.##配信判定##./
SNJ0560B:
    CALL SCVMSG.TOKCLIBO,PARA-('ﾊｲｼﾝ  ﾊﾝﾃｲ     START')
    ?FGLOOP  := '0'

    WHILE  ?FGLOOP = '0'
      DO
        CALL WAITPG.TOKELIB,PARA-('000030')  /. 30秒待ち ./

        ?STEP := 'SNJ1001B'
        ?MSGX := '***   '  && ?STEP   &&   '        ***'
        SNDMSG   ?MSGX,TO-XCTL

        OVRF FILE-ANSERBMS,TOFILE-?ANSERF
        OVRF FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB

/. パラメータ ./
/.   出力                         ./
/.     1.アンサーＦ終了ステータス ./
/.     2.配信判定                 ./

        CALL PGM-SNJ1001B.TOKELIBO,
             PARA-(?JDATE,?JTIME,?TORICD,?ANSSTS,?SYKEKA)

        ?PGMEC := @PGMEC
        ?PGMEM := @PGMEM
        IF  ?PGMEC ^= 0 THEN  /.PG実行異常./
            ?JSNFLG := '9'
            ?KEKA   := 'K300'
            ?FGLOOP := 'E'
            BREAK
        END

    /.  ?MSGX := '## TST ANSSTS=' && ?ANSSTS
        SNDMSG  ?MSGX,TO-XCTL
        ?MSGX := '## TST SYKEKA=' && ?SYKEKA
        SNDMSG  ?MSGX,TO-XCTL  ./

        IF  ?SYKEKA = ' ' THEN  /.配信済み./
            CASE  ?ANSSTS OF
              # '000 ' #  /.正常受信./
                ?JSNFLG := '1'

              # '001 ' #  /.データゼロ件./
                ?JSNFLG := '1'
                ?KEKA   := ?ANSSTS

              ELSE        /.異常受信./
                ?JSNFLG := '9'
                ?KEKA   := ?ANSSTS
            END

            ?FGLOOP := 'E'
            BREAK
        END
    END


/.## 配信終了時間取得 ##./
    ?JIKAN   := @STIME
    ?JSNTIME := %SBSTR(?JIKAN,1,4)

/.##ｴﾗｰ判断##./
ERRCHK2:
    CALL SCVMSG.TOKCLIBO,PARA-('ﾊｲｼﾝ  ﾊﾝﾃｲ CHK START')
    ?STEP := 'ERRCHK2 '
    ?MSGX := '***   '  && ?STEP   &&   '        ***'
    SNDMSG  ?MSGX,TO-XCTL

    ?MSGX := '## 結果 = ' && ?KEKA
    SNDMSG  ?MSGX,TO-XCTL

    IF  ?KEKA ^= '0000'  THEN

        CASE  ?KEKA OF
          # '001 ' #   /. データゼロ件　./
            GOTO  RTN

          ELSE        /.異常受信./
            ?JSNTIME := '0000'
            GOTO  ABEND
          END
    END

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
/.     5.受信状況ＦＬＧ      ./
/.     6.変換状況ＦＬＧ      ./
/.     7.処理結果コード      ./
    CALL    PGM-SNJ0720B.TOKELIBO,PARA-(?JDATE,?JTIME,?TORICD,
            ?JSNTIME,?JSNFLG,?HENFLG,?KEKA)
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF ?PGMEC ^= 0 THEN
       ?KEKA := 'K028'
       GOTO  ABEND
    END

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:
/.##結果ﾘｽﾄ発行##./
    CALL SCVMSG.TOKCLIBO,PARA-('ｹｯｶ LST        START')
/. パラメータ ./
/.   入力                          ./
/.     1.受信日                    ./
/.     2.受信時間                  ./
/.     3.取引先コード              ./
/.     4.結果コード                ./
    SBMJOB JOB-KEKALISI,JOBD-CVCS.XUCL,JOBK-@B,
           PGM-SNJ0590L.TOKELIBO,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
           @S/@T,LIBL-TOKFLIB/TOKELIB/TOKJLIB/TOKELIBO,
           PARA-(?JDATE,?JTIME,?TORICD,?KEKA)

    CALL SCVMSG.TOKCLIBO,PARA-('ｾｲｼﾞｮｳ ｼｭｳﾘｮｳ CBMSHAI')

/.##回線管理マスタ　使用フラグ、使用取引先クリア ./
    CALL SCVMSG.TOKCLIBO,PARA-('ｶｲｾﾝ ｶﾝﾘﾏｽﾀ ﾌﾗｸﾞｼｮｷｶ')

    OVRF  FILE-JSMKAIL1,TOFILE-JSMKAIL1.TOKJLIB
    CALL SNJ9080B.TOKELIBO,PARA-(?KAISHU,?KSNO)

/.##当日スケジュールマスタ、実行完了フラグ更新./
    CALL SCVMSG.TOKCLIBO,PARA-('ｼﾞｯｺｳｶﾝﾘｮｳFLG ｺｳｼﾝ  ')

/.    処理結果設定（0000:正常終了を設定) ./
    ?KEKA  :=  '0000'

    OVRF  FILE-JSMKAIL1,TOFILE-JSMKAIL1.TOKJLIB
    CALL SNJ9110B.TOKELIBO,PARA-(?JDATE,?JTIME,
                                    ?TORICD,?SYKEKA,?KEKA)

/.##ﾛｲﾔﾙ出荷初期化##./
    IF  ?TORICD = '00051649'  THEN
        CNVFILE FILE-PCCHK4.TOKWLIB,TOFILE-PCCHK4.ONLBLIB,
                ADD-@NO,BF-1
    END
/.##ｹｰﾖｰ出荷初期化##./
    IF  ?TORICD = '00000173'  THEN
        CNVFILE FILE-KENCHKF.TOKWLIB,TOFILE-KENCHKF.TOKFLIB,
                ADD-@NO,BF-1
    END

/.##ﾊﾞﾛｰ出荷初期化  2013/02/12  ##./
    IF  ?TORICD = '00013020'  THEN
        CLRFILE FILE-?JFILNM
    END
/.##ﾊﾞﾛｰ植物出荷初期化  2013/03/22  ##./
    IF  ?TORICD = '00013052'  THEN
        CLRFILE FILE-?JFILNM
    END

/.##セキチュー資材出荷初期化  2018/03/13  ##./
    IF  ?TORICD = '00321717'  THEN
        CLRFILE FILE-?JFILNM
    END

/.##セキチュー植物出荷初期化  2018/03/13  ##./
    IF  ?TORICD = '00926061'  THEN
        CLRFILE FILE-?JFILNM
    END

/.##ムサシ出荷初期化  2018/05/18  ##./
    IF  ?TORICD = '00116501'  THEN
        CLRFILE FILE-?JFILNM
    END

/.##山新出荷初期化  2018/09/10  ##./
    IF  ?TORICD = '00001041'  THEN
        CLRFILE FILE-?JFILNM
    END

/.##ｶｲﾝｽﾞ出荷初期化##./
    IF  ?TORICD = '00921084'  THEN
        CNVFILE FILE-PCCHKC.TOKWLIB,TOFILE-PCCHKC.ONLBLIB,
                ADD-@NO,BF-1
                ?MSGX := '## ｶｲﾝｽﾞ初期化 ##'
                SNDMSG ?MSGX,TO-XCTL
    END

/.##ジュンテンドー出荷初期化  2019/04/19  ##./
    IF  ?TORICD = '00005116'  THEN
        CLRFILE FILE-?JFILNM
    END

/.##島忠出荷初期化  2020/07/29  ##./
    IF  ?TORICD = '00072028'  THEN
        CLRFILE FILE-?JFILNM
    END

/.##ビバホーム出荷初期化  2021/10/06  ##./
    IF  ?TORICD = '00038709'  THEN
        CLRFILE FILE-?JFILNM
    END

/.##ジョイフルエーケー出荷初期化  2022/01/06  ##./
    IF  ?TORICD = '00003043'  THEN
        CLRFILE FILE-?JFILNM
    END

/.##アレンザ（ダイユー）出荷初期化  2023/01/20  ##./
    IF  ?TORICD = '00751225'  THEN
        CLRFILE FILE-?JFILNM
    END

/.##アレンザ（日敷）出荷初期化  2023/01/20  ##./
    IF  ?TORICD = '00781225'  THEN
        CLRFILE FILE-?JFILNM
    END

/.##アレンザ（タイム）出荷初期化  2023/01/20  ##./
    IF  ?TORICD = '00761995'  THEN
        CLRFILE FILE-?JFILNM
    END

/.##ハンズマン出荷初期化  2023/12/12  ##./
    IF  ?TORICD = '00000616'  THEN
        CLRFILE FILE-?JFILNM
    END

/.##ＨＩひろせ出荷初期化  2024/11/20  ##./
    IF  ?TORICD = '00031100'  THEN
        CLRFILE FILE-?JFILNM
    END

/.##カインズ出荷初期化  2023/06/08  ##./
    IF  ?TORICD = '00921084'  THEN
        CLRFILE FILE-?JFILNM
    END

    ?MSGX := '***   ' && ?PGMID && ' END    ***'
    SNDMSG ?MSGX,TO-XCTL
    RETURN PGMEC-@PGMEC


/.##ﾌﾟﾛｸﾞﾗﾑ正常終了ゼロ件／リダイアルエラー##./
RTN1:
/.##実行ログ更新##./
    CALL SCVMSG.TOKCLIBO,PARA-('ｼｮﾘｹｯｶ LOG SET START')
    DEFLIBL TOKELIB/TOKFLIB/TOKJLIB
    OVRF  FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB

/. パラメータ ./
/.   入力                    ./
/.     1.日付                ./
/.     2.時刻                ./
/.     3.取引先コード        ./
/.     4.受信終了時刻        ./
/.     5.受信状況ＦＬＧ      ./
/.     6.変換状況ＦＬＧ      ./
/.     7.処理結果コード      ./
    CALL    PGM-SNJ0720B.TOKELIBO,PARA-(?JDATE,?JTIME,?TORICD,
            ?JSNTIME,?JSNFLG,?HENFLG,?KEKA)

/.##回線管理マスタ　使用フラグ、使用取引先クリア ./
    CALL SCVMSG.TOKCLIBO,PARA-('ｶｲｾﾝ ｶﾝﾘﾏｽﾀ ﾌﾗｸﾞｼｮｷｶ')

    OVRF  FILE-JSMKAIL1,TOFILE-JSMKAIL1.TOKJLIB
    CALL SNJ9080B.TOKELIBO,PARA-(?KAISHU,?KSNO)

/.##ﾛｲﾔﾙ出荷初期化##./
    IF  ?TORICD = '00051649'  THEN
        CNVFILE FILE-PCCHK4.TOKWLIB,TOFILE-PCCHK4.ONLBLIB,
                ADD-@NO,BF-1
    END
/.##ｹｰﾖｰ出荷初期化##./
    IF  ?TORICD = '00000173'  THEN
        CNVFILE FILE-KENCHKF.TOKWLIB,TOFILE-KENCHKF.TOKFLIB,
                ADD-@NO,BF-1
    END
/.##ｶｲﾝｽﾞ出荷初期化##./
    IF  ?TORICD = '00921084'  THEN
        CNVFILE FILE-PCCHKC.TOKWLIB,TOFILE-PCCHKC.ONLBLIB,
                ADD-@NO,BF-1
                ?MSGX := '## ｶｲﾝｽﾞ初期化 ##'
                SNDMSG ?MSGX,TO-XCTL
    END

    ?MSGX := '***   ' && ?PGMID && ' END    ***'
    SNDMSG ?MSGX,TO-XCTL
    RETURN PGMEC-@PGMEC

    CALL SCVMSG.TOKCLIBO,PARA-('0ｹﾝ/ﾘﾀﾞｲﾔﾙEND HENTEL')
    ?MSGX := '***   ' && ?PGMID && ' END    ***'
    SNDMSG ?MSGX,TO-XCTL
    RETURN PGMEC-@PGMEC


/.##ﾌﾟﾛｸﾞﾗﾑ異常終了##./
ABEND:
/.##実行ログ更新##./
    CALL SCVMSG.TOKCLIBO,PARA-('ｼｮﾘｹｯｶ LOG SET START')
    DEFLIBL TOKELIB/TOKFLIB/TOKJLIB
    OVRF  FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB

/. パラメータ ./
/.   入力                    ./
/.     1.日付                ./
/.     2.時刻                ./
/.     3.取引先コード        ./
/.     4.受信終了時刻        ./
/.     5.受信状況ＦＬＧ      ./
/.     6.変換状況ＦＬＧ      ./
/.     7.処理結果コード      ./
    CALL    PGM-SNJ0720B.TOKELIBO,PARA-(?JDATE,?JTIME,?TORICD,
            ?JSNTIME,?JSNFLG,?HENFLG,?KEKA)

/.##結果ﾘｽﾄ発行##./
    CALL SCVMSG.TOKCLIBO,PARA-('ｹｯｶ LST        START')
/. パラメータ ./
/.   入力                          ./
/.     1.受信日                    ./
/.     2.受信時間                  ./
/.     3.取引先コード              ./
/.     4.結果コード                ./
    SBMJOB JOB-KEKALSI,JOBD-CVCS.XUCL,JOBK-@B,
           PGM-SNJ0590L.TOKELIBO,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
           @S/@T,LIBL-TOKFLIB/TOKELIB/TOKJLIB/TOKELIBO,
           PARA-(?JDATE,?JTIME,?TORICD,?KEKA)

    CALL SCVMSG.TOKCLIBO,PARA-('ｲｼﾞｮｳ ｼｭｳﾘｮｳ  CBMSHAI')

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
    SNDMSG MSG-'＃＜配信処理において異常終了が発生　＞＃',
           TO-XCTL
    SNDMSG MSG-'＃　エラー情報を回収し、至急、　　　　＃',
           TO-XCTL
    SNDMSG MSG-'＃　ナブ・アシストまで連絡して下さい。＃',
           TO-XCTL
    SNDMSG MSG-'＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃',
           TO-XCTL
    RETURN PGMEC-@PGMEC
