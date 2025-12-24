****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩ　　　　　　　　　　*
*    業務名　　　　　　　：　ナフコ出荷支援　　　　　　　　　　*
*    モジュール名　　　　：　発注一覧　　　　　　　　　　　　　*
*    　　　　　　　　　　　　ＣＳＶデータ作成　　　　　　　　  *
*    作成日／作成者　　　：　2019/12/05 ASS TAKAHASHI          *
*    処理概要　　　　　　：　発注一覧ワークより　　　　　　　　*
*                            ＰＣへ転送するＣＳＶデータを　　　*
*                            出力する。　　　　　              *
*    更新日／更新者　　　：　                                  *
*                            　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3942V.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/12/05.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*発注一覧ワーク
     SELECT   NFWKXXX4  ASSIGN    TO        DA-01-VI-NFWKXXX4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       IS   NWK-A11
                                                 NWK-A25
                                                 NWK-A26
                                                 NWK-A23
                                                 NWK-A24
                        FILE      STATUS    NWK-STATUS.
*発注一覧ＣＳＶ
     SELECT   NFHACXXX  ASSIGN    TO        DA-01-NFHACXXX
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CSV-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*発注一覧ワーク
******************************************************************
 FD  NFWKXXX4           LABEL RECORD   IS   STANDARD.
**     COPY     NFWKXXXF
**              DISJOINING XXX JOINING NWK AS PREFIX.
     COPY     NFWKXXXF   OF        XFDLIB
              JOINING   NWK  AS   PREFIX.
*
******************************************************************
*発注一覧ＣＳＶ
******************************************************************
 FD  NFHACXXX            LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS  1       RECORDS.
**     COPY     HACXXX1
**              DISJOINING XXX JOINING CSV AS PREFIX.
     COPY     HACXXX1   OF        XFDLIB
              JOINING   CSV       PREFIX.

*****************************************************************
*
 WORKING-STORAGE        SECTION.
*
*振分ＣＳＶ（明細）マルチレイアウト用ＣＯＰＹ句
* 1.帳票タイトル行
**     COPY     HACXXX1
**              DISJOINING XXX JOINING CSV1 AS PREFIX.
     COPY     HACXXX1   OF        XFDLIB
              JOINING   CSV1 AS   PREFIX.
* 2.項目タイトル行1
**     COPY     HACXXX2
**              DISJOINING XXX JOINING CSV2 AS PREFIX.
     COPY     HACXXX2   OF        XFDLIB
              JOINING   CSV2 AS   PREFIX.
* 3.明細行
**     COPY     HACXXX3
**              DISJOINING XXX JOINING CSV3 AS PREFIX.
     COPY     HACXXX3   OF        XFDLIB
              JOINING   CSV3 AS   PREFIX.
*
*ＳＴＡＴＵＳ領域
 01  WK-ST.
     03  NWK-STATUS          PIC  X(02).
     03  CSV-STATUS          PIC  X(02).
*フラグ領域
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
*カウンター領域
 01  WK-CNT.
     03  RD-CNT              PIC  9(08)     VALUE  ZERO.
     03  SEL-CNT             PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT             PIC  9(08)     VALUE  ZERO.
*
*数字編集用
 01  WK-TANKA-X.
     03  WK-TANKA            PIC  9(07).99.
 01  WK-RITU-X.
     03  WK-RITU             PIC  9(03).9.
*システム日付編集領域
 01  WK-AREA.
     03  SYS-DATE            PIC  9(06).
     03  SYS-DATEW           PIC  9(08).
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SSY3942V".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY3942V".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY3942V".
         05  FILLER          PIC  X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  AB-FILE         PIC  X(08).
         05  FILLER          PIC  X(06)  VALUE " ST = ".
         05  AB-STS          PIC  X(02).
         05  FILLER          PIC  X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  FILLER          PIC  X(07)  VALUE " SEC = ".
         05  S-NAME          PIC  X(30).
     03  MSG-IN.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " 元データ読込件数   = ".
         05  MSG-IN01        PIC  ZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " CSVデータ抽出件数  = ".
         05  MSG-OUT01       PIC  ZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
*日付変換サブルーチン用領域
 01  LINK-AREA.
     03  LINK-IN-KBN         PIC   X(01).
     03  LINK-IN-YMD6        PIC   9(06).
     03  LINK-IN-YMD8        PIC   9(08).
     03  LINK-OUT-RET        PIC   X(01).
     03  LINK-OUT-YMD8       PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-HAC-FROM           PIC   9(08).
 01  PARA-HAC-TO             PIC   9(08).
 01  PARA-JANCD              PIC   X(13).
 01  PARA-TEN-FROM           PIC   9(03).
 01  PARA-TEN-TO             PIC   9(03).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION  USING
                                             PARA-HAC-FROM
                                             PARA-HAC-TO
                                             PARA-JANCD
                                             PARA-TEN-FROM
                                             PARA-TEN-TO.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFWKXXX4.
     MOVE      "NFWKXXX4"   TO   AB-FILE.
     MOVE      NWK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.

 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFHACXXX.
     MOVE      "NFHACXXX"   TO   AB-FILE.
     MOVE      CSV-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.

 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
*
*初期処理
     PERFORM  INIT-SEC.
*ＣＳＶ出力
     PERFORM  MAIN1-SEC
              UNTIL     END-FLG   =  "END".
*終了処理
     PERFORM  END-SEC.
     STOP  RUN.
*
 GENERAL-PROCESS-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.

     MOVE     "INIT-SEC"          TO   S-NAME.

     DISPLAY  MSG-START UPON CONS.

     OPEN     INPUT     NFWKXXX4.
     OPEN     OUTPUT    NFHACXXX.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
         MOVE    "20"       TO   SYS-DATEW(1:2).
         MOVE    SYS-DATE   TO   SYS-DATEW(3:6).
*
*発注一覧ワーク読込み
     PERFORM NFWKXXX4-READ-SEC.
*
*終了判定
     IF   END-FLG  =  "END"
          DISPLAY "＃＃出力対象データ無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4010     TO    PROGRAM-STATUS
     ELSE
*  発注一覧ＣＳＶ　　編集・出力　　 1.帳票タイトル行
          PERFORM  CHGXXX1-SET-SEC
          WRITE    CSV-REC   FROM      CSV1-REC
*  発注一覧ＣＳＶ　　編集・出力　　 2.項目タイトル行
          PERFORM  CHGXXX2-SET-SEC
          WRITE    CSV-REC   FROM      CSV2-REC
*  発注一覧ＣＳＶ　　初期値設定　　 3.明細
          PERFORM  CHGXXX3-INIT-SEC
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　発注一覧ワーク読込　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 NFWKXXX4-READ-SEC    SECTION.
*
     MOVE    "NFWKXXX4-READ-SEC"    TO   S-NAME.
*
 NFWKXXX4-READ-01.
     READ     NFWKXXX4
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  NFWKXXX4-READ-EXIT
     END-READ.
*
     ADD      1     TO     RD-CNT.
*
*条件判定
*  発注日
     IF      PARA-HAC-FROM  <=  NWK-A25
        AND  PARA-HAC-TO    >=  NWK-A25
             CONTINUE
     ELSE
             GO   TO    NFWKXXX4-READ-01
     END-IF.
*  ＪＡＮコード
     IF      PARA-JANCD      =  SPACE
        OR   PARA-JANCD      =  NWK-A44
             CONTINUE
     ELSE
             GO   TO    NFWKXXX4-READ-01
     END-IF.
*  店舗コード
     IF      PARA-TEN-FROM  <=  NWK-A83
        AND  PARA-TEN-TO    >=  NWK-A83
             CONTINUE
     ELSE
             GO   TO    NFWKXXX4-READ-01
     END-IF.
*
     ADD      1     TO     SEL-CNT.
*
 NFWKXXX4-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理１　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN1-SEC     SECTION.
*
     MOVE    "MAIN1-SEC"     TO      S-NAME.
*
*  発注一覧ＣＳＶ　　編集　　　 3.明細行
     PERFORM  CHGXXX3-SET-SEC.
*    レコード出力
     WRITE    CSV-REC   FROM  CSV3-REC.
*    出力件数カウント
     ADD      1         TO    OUT-CNT.
*
*    発注一覧ワーク読込み
     PERFORM NFWKXXX4-READ-SEC.
*
 MAIN1-EXIT.
     EXIT.
****************************************************************
*発注一覧ＣＳＶ  SET   1.帳票タイトル行
****************************************************************
 CHGXXX1-SET-SEC     SECTION.
*
     MOVE   "CHGXXX1-SET-SEC"  TO  S-NAME.
*
     MOVE    SPACE                                TO CSV1-REC.
*
     MOVE    X"28"                                TO CSV1-CS00.
     MOVE    NC"ナフコ　発注一覧"                 TO CSV1-C00.
     MOVE    X"29"                                TO CSV1-CE00.
     MOVE    ","                                  TO CSV1-CK00.
*
 CHGXXX11-SET-EXIT.
     EXIT.
****************************************************************
*発注一覧ＣＳＶ   SET   2.帳票タイトル行
****************************************************************
 CHGXXX2-SET-SEC     SECTION.
*
     MOVE   "CHGXXX2-SET-SEC"  TO  S-NAME.
*
     MOVE    SPACE                                TO CSV2-REC.
*
     MOVE    X"28"                                TO CSV2-KS01.
     MOVE    NC"店舗コード"                       TO CSV2-K01.
     MOVE    X"29"                                TO CSV2-KE01.
     MOVE    ","                                  TO CSV2-KK01.
*
     MOVE    X"28"                                TO CSV2-KS02.
     MOVE    NC"店舗名"                           TO CSV2-K02.
     MOVE    X"29"                                TO CSV2-KE02.
     MOVE    ","                                  TO CSV2-KK02.
*
     MOVE    X"28"                                TO CSV2-KS03.
     MOVE    NC"伝票区分"                         TO CSV2-K03.
     MOVE    X"29"                                TO CSV2-KE03.
     MOVE    ","                                  TO CSV2-KK03.
*
     MOVE    X"28"                                TO CSV2-KS04.
     MOVE    NC"伝票番号"                         TO CSV2-K04.
     MOVE    X"29"                                TO CSV2-KE04.
     MOVE    ","                                  TO CSV2-KK04.
*
     MOVE    X"28"                                TO CSV2-KS05.
     MOVE    NC"行"                               TO CSV2-K05.
     MOVE    X"29"                                TO CSV2-KE05.
     MOVE    ","                                  TO CSV2-KK05.
*
     MOVE    X"28"                                TO CSV2-KS06.
     MOVE    NC"発注日"                           TO CSV2-K06.
     MOVE    X"29"                                TO CSV2-KE06.
     MOVE    ","                                  TO CSV2-KK06.
*
     MOVE    X"28"                                TO CSV2-KS07.
     MOVE    NC"納品予定日"                       TO CSV2-K07.
     MOVE    X"29"                                TO CSV2-KE07.
     MOVE    ","                                  TO CSV2-KK07.
*
     MOVE    X"28"                                TO CSV2-KS08.
     MOVE    NC"発注データ有効期限"               TO CSV2-K08.
     MOVE    X"29"                                TO CSV2-KE08.
     MOVE    ","                                  TO CSV2-KK08.
*
     MOVE    X"28"                                TO CSV2-KS09.
     MOVE    NC"ＥＤＩ発注区分"                   TO CSV2-K09.
     MOVE    X"29"                                TO CSV2-KE09.
     MOVE    ","                                  TO CSV2-KK09.
*
     MOVE    X"28"                                TO CSV2-KS10.
     MOVE    NC"発注形態区分"                     TO CSV2-K10.
     MOVE    X"29"                                TO CSV2-KE10.
     MOVE    ","                                  TO CSV2-KK10.
*
     MOVE    X"28"                                TO CSV2-KS11.
     MOVE    NC"発注形態名"                       TO CSV2-K11.
     MOVE    X"29"                                TO CSV2-KE11.
     MOVE    ","                                  TO CSV2-KK11.
*
     MOVE    X"28"                                TO CSV2-KS12.
     MOVE    NC"本部発注区分"                     TO CSV2-K12.
     MOVE    X"29"                                TO CSV2-KE12.
     MOVE    ","                                  TO CSV2-KK12.
*
     MOVE    X"28"                                TO CSV2-KS13.
     MOVE    NC"部門コード"                       TO CSV2-K13.
     MOVE    X"29"                                TO CSV2-KE13.
     MOVE    ","                                  TO CSV2-KK13.
*
     MOVE    X"28"                                TO CSV2-KS14.
     MOVE    NC"部門名"                           TO CSV2-K14.
     MOVE    X"29"                                TO CSV2-KE14.
     MOVE    ","                                  TO CSV2-KK14.
*
     MOVE    X"28"                                TO CSV2-KS15.
     MOVE    NC"ライン"                           TO CSV2-K15.
     MOVE    X"29"                                TO CSV2-KE15.
     MOVE    ","                                  TO CSV2-KK15.
*
     MOVE    X"28"                                TO CSV2-KS16.
     MOVE    NC"クラス"                           TO CSV2-K16.
     MOVE    X"29"                                TO CSV2-KE16.
     MOVE    ","                                  TO CSV2-KK16.
*
     MOVE    X"28"                                TO CSV2-KS17.
     MOVE    NC"ロケーション"                     TO CSV2-K17.
     MOVE    X"29"                                TO CSV2-KE17.
     MOVE    ","                                  TO CSV2-KK17.
*
     MOVE    X"28"                                TO CSV2-KS18.
     MOVE    NC"ＪＡＮＣＤ"                       TO CSV2-K18.
     MOVE    X"29"                                TO CSV2-KE18.
     MOVE    ","                                  TO CSV2-KK18.
*
     MOVE    X"28"                                TO CSV2-KS19.
     MOVE    NC"商品ＣＤ"                         TO CSV2-K19.
     MOVE    X"29"                                TO CSV2-KE19.
     MOVE    ","                                  TO CSV2-KK19.
*
     MOVE    X"28"                                TO CSV2-KS20.
     MOVE    NC"オプション"                       TO CSV2-K20.
     MOVE    X"29"                                TO CSV2-KE20.
     MOVE    ","                                  TO CSV2-KK20.
*
     MOVE    X"28"                                TO CSV2-KS21.
     MOVE    NC"品名"                             TO CSV2-K21.
     MOVE    X"29"                                TO CSV2-KE21.
     MOVE    ","                                  TO CSV2-KK21.
*
     MOVE    X"28"                                TO CSV2-KS22.
     MOVE    NC"規格名"                           TO CSV2-K22.
     MOVE    X"29"                                TO CSV2-KE22.
     MOVE    ","                                  TO CSV2-KK22.
*
     MOVE    X"28"                                TO CSV2-KS23.
     MOVE    NC"発注数"                           TO CSV2-K23.
     MOVE    X"29"                                TO CSV2-KE23.
     MOVE    ","                                  TO CSV2-KK23.
*
     MOVE    X"28"                                TO CSV2-KS24.
     MOVE    NC"発注単位名"                         TO CSV2-K24.
     MOVE    X"29"                                TO CSV2-KE24.
     MOVE    ","                                  TO CSV2-KK24.
*
     MOVE    X"28"                                TO CSV2-KS25.
     MOVE    NC"総額取引区分"                     TO CSV2-K25.
     MOVE    X"29"                                TO CSV2-KE25.
     MOVE    ","                                  TO CSV2-KK25.
*
     MOVE    X"28"                                TO CSV2-KS26.
     MOVE    NC"原単価（税抜）"                   TO CSV2-K26.
     MOVE    X"29"                                TO CSV2-KE26.
     MOVE    ","                                  TO CSV2-KK26.
*
     MOVE    X"28"                                TO CSV2-KS27.
     MOVE    NC"原単価（税込）"                   TO CSV2-K27.
     MOVE    X"29"                                TO CSV2-KE27.
     MOVE    ","                                  TO CSV2-KK27.
*
     MOVE    X"28"                                TO CSV2-KS28.
     MOVE    NC"原価金額（税抜）"                 TO CSV2-K28.
     MOVE    X"29"                                TO CSV2-KE28.
     MOVE    ","                                  TO CSV2-KK28.
*
     MOVE    X"28"                                TO CSV2-KS29.
     MOVE    NC"原価金額（税込）"                 TO CSV2-K29.
     MOVE    X"29"                                TO CSV2-KE29.
     MOVE    ","                                  TO CSV2-KK29.
*
     MOVE    X"28"                                TO CSV2-KS30.
     MOVE    NC"税額"                             TO CSV2-K30.
     MOVE    X"29"                                TO CSV2-KE30.
     MOVE    ","                                  TO CSV2-KK30.
*
     MOVE    X"28"                                TO CSV2-KS31.
     MOVE    NC"税区"                             TO CSV2-K31.
     MOVE    X"29"                                TO CSV2-KE31.
     MOVE    ","                                  TO CSV2-KK31.
*
     MOVE    X"28"                                TO CSV2-KS32.
     MOVE    NC"税率"                             TO CSV2-K32.
     MOVE    X"29"                                TO CSV2-KE32.
     MOVE    ","                                  TO CSV2-KK32.
*
     MOVE    X"28"                                TO CSV2-KS33.
     MOVE    NC"売単価（税抜）"                   TO CSV2-K33.
     MOVE    X"29"                                TO CSV2-KE33.
     MOVE    ","                                  TO CSV2-KK33.
*
     MOVE    X"28"                                TO CSV2-KS34.
     MOVE    NC"売単価（税込）"                   TO CSV2-K34.
     MOVE    X"29"                                TO CSV2-KE34.
     MOVE    ","                                  TO CSV2-KK34.
*
     MOVE    X"28"                                TO CSV2-KS35.
     MOVE    NC"特価区分"                         TO CSV2-K35.
     MOVE    X"29"                                TO CSV2-KE35.
     MOVE    ","                                  TO CSV2-KK35.
*
     MOVE    X"28"                                TO CSV2-KS36.
     MOVE    NC"ＰＢ区分"                         TO CSV2-K36.
     MOVE    X"29"                                TO CSV2-KE36.
     MOVE    ","                                  TO CSV2-KK36.
*
     MOVE    X"28"                                TO CSV2-KS37.
     MOVE    NC"原価区分"                         TO CSV2-K37.
     MOVE    X"29"                                TO CSV2-KE37.
     MOVE    ","                                  TO CSV2-KK37.
*
     MOVE    X"28"                                TO CSV2-KS38.
     MOVE    NC"用度品区分"                       TO CSV2-K38.
     MOVE    X"29"                                TO CSV2-KE38.
     MOVE    ","                                  TO CSV2-KK38.
*
     MOVE    X"28"                                TO CSV2-KS39.
     MOVE    NC"納期回答区分"                     TO CSV2-K39.
     MOVE    X"29"                                TO CSV2-KE39.
     MOVE    ","                                  TO CSV2-KK39.
*
     MOVE    X"28"                                TO CSV2-KS40.
     MOVE    NC"納品先店舗コード"                 TO CSV2-K40.
     MOVE    X"29"                                TO CSV2-KE40.
     MOVE    ","                                  TO CSV2-KK40.
*
     MOVE    X"28"                                TO CSV2-KS41.
     MOVE    NC"納品先店舗名"                     TO CSV2-K41.
     MOVE    X"29"                                TO CSV2-KE41.
     MOVE    ","                                  TO CSV2-KK41.
*
     MOVE    X"28"                                TO CSV2-KS42.
     MOVE    NC"納品場所コード"                   TO CSV2-K42.
     MOVE    X"29"                                TO CSV2-KE42.
     MOVE    ","                                  TO CSV2-KK42.
*
     MOVE    X"28"                                TO CSV2-KS43.
     MOVE    NC"納品場所名"                       TO CSV2-K43.
     MOVE    X"29"                                TO CSV2-KE43.
     MOVE    ","                                  TO CSV2-KK43.
*
     MOVE    X"28"                                TO CSV2-KS44.
     MOVE    NC"センター経由区分"                 TO CSV2-K44.
     MOVE    X"29"                                TO CSV2-KE44.
     MOVE    ","                                  TO CSV2-KK44.
*
     MOVE    X"28"                                TO CSV2-KS45.
     MOVE    NC"センターコード"                   TO CSV2-K45.
     MOVE    X"29"                                TO CSV2-KE45.
     MOVE    ","                                  TO CSV2-KK45.
*
     MOVE    X"28"                                TO CSV2-KS46.
     MOVE    NC"センター名"                       TO CSV2-K46.
     MOVE    X"29"                                TO CSV2-KE46.
     MOVE    ","                                  TO CSV2-KK46.
*
*    MOVE    ALL ","                              TO CSV2-KK16.
*
 CHGXXX2-SET-EXIT.
     EXIT.
****************************************************************
*発注一覧ＣＳＶ   初期値設定   3.明細行
****************************************************************
 CHGXXX3-INIT-SEC    SECTION.
*
     MOVE     "CHGXXX3-INIT-SEC" TO  S-NAME.
*
     MOVE     SPACE          TO   CSV3-REC.
*制御バイト
     MOVE    X"28"           TO   CSV3-H12
                                  CSV3-H30
                                  CSV3-H36
                                  CSV3-H48
                                  CSV3-H52
                                  CSV3-H58
                                  CSV3-H84
                                  CSV3-H89
                                  CSV3-H96.
     MOVE    X"29"           TO   CSV3-H14
                                  CSV3-H32
                                  CSV3-H38
                                  CSV3-H50
                                  CSV3-H54
                                  CSV3-H60
                                  CSV3-H86
                                  CSV3-H91
                                  CSV3-H98.
*カンマ
     MOVE    ","             TO   CSV3-C11
                                  CSV3-C14
                                  CSV3-C22
                                  CSV3-C23
                                  CSV3-C24
                                  CSV3-C25
                                  CSV3-C26
                                  CSV3-C27
                                  CSV3-C28
                                  CSV3-C29
                                  CSV3-C32
                                  CSV3-C34
                                  CSV3-C35
                                  CSV3-C38
                                  CSV3-C40
                                  CSV3-C41
                                  CSV3-C42
                                  CSV3-C44
                                  CSV3-C45
                                  CSV3-C46
                                  CSV3-C50
                                  CSV3-C54
                                  CSV3-C56
                                  CSV3-C60
                                  CSV3-C62
                                  CSV3-C63
                                  CSV3-C64
                                  CSV3-C65
                                  CSV3-C66
                                  CSV3-C67
                                  CSV3-C68
                                  CSV3-C69
                                  CSV3-C70
                                  CSV3-C71
                                  CSV3-C72
                                  CSV3-C73
                                  CSV3-C74
                                  CSV3-C75
                                  CSV3-C76
                                  CSV3-C83
                                  CSV3-C86
                                  CSV3-C88
                                  CSV3-C91
                                  CSV3-C94
                                  CSV3-C95
                                  CSV3-C98.
*
 CHGXXX3-INIT-EXIT.
     EXIT.
****************************************************************
*発注一覧ＣＳＶ   SET   3.明細行
****************************************************************
 CHGXXX3-SET-SEC     SECTION.
*
     MOVE     "CHGXXX3-SET-SEC"  TO  S-NAME.
*
*店舗コード
     MOVE    NWK-A11         TO   CSV3-H11.
*店舗名漢字
     MOVE    NWK-A13         TO   CSV3-H13.
*伝票区分
     MOVE    NWK-A22         TO   CSV3-H22.
*伝票番号
     MOVE    NWK-A23         TO   CSV3-H23.
*行番号
     MOVE    NWK-A24         TO   CSV3-H24.
*発注日
     MOVE    NWK-A25         TO   CSV3-H25.
*納品予定日
     MOVE    NWK-A26         TO   CSV3-H26.
*発注データ有効期限
     MOVE    NWK-A27         TO   CSV3-H27.
*ＥＤＩ発注区分
     MOVE    NWK-A28         TO   CSV3-H28.
*発注形態区分
     MOVE    NWK-A29         TO   CSV3-H29.
*発注形態名称漢字
     MOVE    NWK-A31         TO   CSV3-H31.
*本部発注区分
     MOVE    NWK-A34         TO   CSV3-H34.
*部門コード
     MOVE    NWK-A35         TO   CSV3-H35.
*部門名漢字
     MOVE    NWK-A37         TO   CSV3-H37.
*ラインコード
     MOVE    NWK-A40         TO   CSV3-H40.
*クラスコード
     MOVE    NWK-A41         TO   CSV3-H41.
*ロケーションコード
     MOVE    NWK-A42         TO   CSV3-H42.
*ＪＡＮコード
     MOVE    NWK-A44         TO   CSV3-H44.
*商品コード
     MOVE    NWK-A45         TO   CSV3-H45.
*オプション使用欄
     MOVE    NWK-A46         TO   CSV3-H46.
*品名漢字
     MOVE    NWK-A49         TO   CSV3-H49.
*規格名漢字
     MOVE    NWK-A53         TO   CSV3-H53.
*発注数
     MOVE    NWK-A56         TO   CSV3-H56.
*発注単位名称漢字
     MOVE    NWK-A59         TO   CSV3-H59.
*総額取引区分
     MOVE    NWK-A62         TO   CSV3-H62.
*原単価（税抜）
     MOVE    NWK-A63         TO   CSV3-H63.
*原単価（税込）
     MOVE    NWK-A64         TO   WK-TANKA.
     MOVE    WK-TANKA-X      TO   CSV3-H64.
*原価金額（税抜）
     MOVE    NWK-A65         TO   CSV3-H65.
*原価金額（税込）
     MOVE    NWK-A66         TO   CSV3-H66.
*税額
     MOVE    NWK-A67         TO   CSV3-H67.
*税区分
     MOVE    NWK-A68         TO   CSV3-H68.
*税率
     MOVE    NWK-A69         TO   WK-RITU.
     MOVE    WK-RITU-X       TO   CSV3-H69.
*売単価（税抜）
     MOVE    NWK-A70         TO   CSV3-H70.
*売単価（税込）
     MOVE    NWK-A71         TO   CSV3-H71.
*特価区分
     MOVE    NWK-A72         TO   CSV3-H72.
*ＰＢ区分
     MOVE    NWK-A73         TO   CSV3-H73.
*原価区分
     MOVE    NWK-A74         TO   CSV3-H74.
*用度品区分
     MOVE    NWK-A75         TO   CSV3-H75.
*納期回答区分
     MOVE    NWK-A76         TO   CSV3-H76.
*納品先店舗コード
     MOVE    NWK-A83         TO   CSV3-H83.
*納品先店舗名漢字
     MOVE    NWK-A85         TO   CSV3-H85.
*納品場所コード
     MOVE    NWK-A88         TO   CSV3-H88.
*納品場所名漢字
     MOVE    NWK-A90         TO   CSV3-H90.
*センター経由区分
     MOVE    NWK-A94         TO   CSV3-H94.
*センターコード
     MOVE    NWK-A95         TO   CSV3-H95.
*センター名漢字
     MOVE    NWK-A97         TO   CSV3-H97.
*
 CHGXXX3-SET-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE     RD-CNT     TO      MSG-IN01.
     MOVE     OUT-CNT    TO      MSG-OUT01.
     DISPLAY  MSG-IN     UPON    CONS.
     DISPLAY  MSG-OUT    UPON    CONS.
*
     CLOSE    NFWKXXX4   NFHACXXX.
*
     DISPLAY  MSG-END UPON CONS.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
