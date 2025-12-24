# SSY9305V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY9305V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩ　出荷　　　　　　　　　　　*
*    サブシステム　　　　：　ＡＭＡＺＯＮ　ＥＤＩ　　　　　　　*
*    モジュール名　　　　：　出荷確認ＣＳＶ出力　　　　　　　　*
*    作成日／作成者　　　：　2020/11/17 NAV INOUE              *
*    処理概要　　　　　　：　出荷確認データより　　　　　　　　*
*                            ＰＣへ転送するＣＳＶデータを　　　*
*                            出力する。　　　　　              *
*    更新日／更新者　　　：　                                  *
*                            　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY9305V.
*                  流用:SSY3942V.TOKSRLIB
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
*出荷確認データ
     SELECT   AMZCHKW1  ASSIGN    TO        DA-01-VI-AMZCHKW1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       IS   CHK-FE01
                                                 CHK-FE02
                                                 CHK-FE03
                                                 CHK-FA08
                                                 CHK-FA04
                                                 CHK-FA03
                                                 CHK-FA09
                                                 CHK-FB01
                                                 CHK-FB02
                        FILE      STATUS    CHK-STATUS.
*出荷確認ＣＳＶ
     SELECT   AMZCHKCS  ASSIGN    TO        DA-01-S-AMZCHKCS
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CSV-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*出荷確認データ
******************************************************************
 FD  AMZCHKW1            LABEL RECORD   IS   STANDARD.
     COPY     AMZCHKW1   OF        XFDLIB
              JOINING    CHK  AS   PREFIX.
*
******************************************************************
*出荷確認ＣＳＶ
******************************************************************
 FD  AMZCHKCS            LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS   1         RECORDS.
     COPY     AMZCHKC1   OF        XFDLIB
              JOINING    CSV       PREFIX.

*****************************************************************
*
 WORKING-STORAGE        SECTION.
*
*出荷確認ＣＳＶ   マルチレイアウト用ＣＯＰＹ句
* 1.ファイルタイトルレコード
     COPY     AMZCHKC1  OF        XFDLIB
              JOINING   CSV1 AS   PREFIX.
* 2.項目タイトルレコード１
     COPY     AMZCHKC2  OF        XFDLIB
              JOINING   CSV2 AS   PREFIX.
* 3.項目タイトルレコード２
     COPY     AMZCHKC3  OF        XFDLIB
              JOINING   CSV3 AS   PREFIX.
* 4.明細レコード
     COPY     AMZCHKC4  OF        XFDLIB
              JOINING   CSV4 AS   PREFIX.
*
*ＳＴＡＴＵＳ領域
 01  WK-ST.
     03  CHK-STATUS          PIC  X(02).
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
         05  ST-PG           PIC  X(08)  VALUE "SSY9305V".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY9305V".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY9305V".
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
 01  PARA-IN-JOBKIND         PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION  USING
                                             PARA-IN-JOBKIND.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   AMZCHKW1.
     MOVE      "AMZCHKW1"   TO   AB-FILE.
     MOVE      CHK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   AMZCHKCS.
     MOVE      "AMZCHKCS"   TO   AB-FILE.
     MOVE      CSV-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
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
*
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     DISPLAY  MSG-START UPON CONS.
*
     DISPLAY  "JOBKIND=" PARA-IN-JOBKIND  UPON CONS.
*
     OPEN     INPUT     AMZCHKW1.
     OPEN     OUTPUT    AMZCHKCS.
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
*出荷確認データ読込み
     PERFORM AMZCHKW1-READ-SEC.
*
*終了判定
     IF   END-FLG  =  "END"
          DISPLAY "＃＃出力対象データ無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4010     TO    PROGRAM-STATUS
     ELSE
*       出荷確認ＣＳＶ編集・出力　 1.ファイルタイトルレコード
          PERFORM  AMZCHKC1-SET-SEC
          WRITE    CSV-REC   FROM      CSV1-REC
*       出荷確認ＣＳＶ編集・出力　 2.項目タイトルレコード１
          PERFORM  AMZCHKC2-SET-SEC
          WRITE    CSV-REC   FROM      CSV2-REC
*       出荷確認ＣＳＶ編集・出力　 3.項目タイトルレコード２
          PERFORM  AMZCHKC3-SET-SEC
          WRITE    CSV-REC   FROM      CSV3-REC
*       出荷確認ＣＳＶ初期値設定　 4.明細レコード
          PERFORM  AMZCHKC4-INIT-SEC
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　出荷確認データ読込　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 AMZCHKW1-READ-SEC    SECTION.
*
     MOVE    "AMZCHKW1-READ-SEC"    TO   S-NAME.
*
 AMZCHKW1-READ-01.
     READ     AMZCHKW1
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  AMZCHKW1-READ-EXIT
     END-READ.
*
     ADD      1     TO     RD-CNT.
*
*条件判定
*  全件対象
     ADD      1     TO     SEL-CNT.
*
 AMZCHKW1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理１　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN1-SEC     SECTION.
*
     MOVE    "MAIN1-SEC"     TO      S-NAME.
*
*  出荷確認ＣＳＶ　　編集　　　 4.明細レコード
     PERFORM  AMZCHKC4-SET-SEC.
*    レコード出力
     WRITE    CSV-REC   FROM  CSV4-REC.
*    出力件数カウント
     ADD      1         TO    OUT-CNT.
*
*    出荷確認データ読込み
     PERFORM AMZCHKW1-READ-SEC.
*
 MAIN1-EXIT.
     EXIT.
****************************************************************
*出荷確認ＣＳＶ  SET   1.ファイルタイトルレコード
****************************************************************
 AMZCHKC1-SET-SEC     SECTION.
*
     MOVE   "AMZCHKC1-SET-SEC"  TO  S-NAME.
*
     MOVE    SPACE                                TO CSV1-REC.
*
     MOVE     X"28"                               TO CSV1-CS00.
     EVALUATE PARA-IN-JOBKIND
       WHEN   "1"
              MOVE NC"ＡＭＡＺＯＮ出荷確認データ" TO CSV1-C00
       WHEN   "2"
              MOVE NC"ＡＭＡＺＯＮ受注確認データ" TO CSV1-C00
       WHEN   "3"
              MOVE NC"ＡＭＡＺＯＮ引当確認データ" TO CSV1-C00
       WHEN   "4"
              MOVE NC"ＡＭＡＺＯＮ検品確認データ" TO CSV1-C00
       WHEN   OTHER
              MOVE NC"ＡＭＡＺＯＮ出荷確認データ" TO CSV1-C00
     END-EVALUATE.
     MOVE     X"29"                               TO CSV1-CE00.
     MOVE     ALL ","                             TO CSV1-CK00.
*
     MOVE     X"28"                               TO CSV1-CS01.
     MOVE     NC"状態："                          TO CSV1-C01.
     MOVE     X"29"                               TO CSV1-CE01.
     MOVE     ","                                 TO CSV1-CK01.
*
     MOVE     X"28"                               TO CSV1-CS02.
*T
     DISPLAY "F001=" CHK-F001 UPON CONS.
*T
     EVALUATE CHK-F001
       WHEN   "1"
              MOVE NC"受注連携　　"               TO CSV1-C02
       WHEN   "2"
              MOVE NC"引当連携　　"               TO CSV1-C02
       WHEN   "3"
              MOVE NC"出荷確定連携"               TO CSV1-C02
       WHEN   OTHER
              MOVE NC"？？？？連携"               TO CSV1-C02
     END-EVALUATE.
     MOVE     X"29"                               TO CSV1-CE02.
     MOVE     ","                                 TO CSV1-CK02.
*
 AMZCHKC1-SET-EXIT.
     EXIT.
****************************************************************
*出荷確認ＣＳＶ   SET   2.項目タイトルレコード１
****************************************************************
 AMZCHKC2-SET-SEC     SECTION.
*
     MOVE   "AMZCHKC2-SET-SEC"  TO  S-NAME.
*
     MOVE    SPACE                                TO CSV2-REC.
*
     MOVE    X"28"                                TO CSV2-FS01.
     MOVE    NC"受注情報"                         TO CSV2-F01.
     MOVE    X"29"                                TO CSV2-FE01.
     MOVE    ALL ","                              TO CSV2-FK01.
*
     MOVE    X"28"                                TO CSV2-FS02.
     MOVE    NC"引当情報"                         TO CSV2-F02.
     MOVE    X"29"                                TO CSV2-FE02.
     MOVE    ALL ","                              TO CSV2-FK02.
*
     MOVE    X"28"                                TO CSV2-FS03.
     MOVE    NC"検品実績"                         TO CSV2-F03.
     MOVE    X"29"                                TO CSV2-FE03.
     MOVE    ALL ","                              TO CSV2-FK03.
*
     MOVE    X"28"                                TO CSV2-FS04.
     EVALUATE PARA-IN-JOBKIND
       WHEN   "1"
              MOVE NC"　　　　　　"               TO CSV2-F04
       WHEN   "2"
              MOVE NC"エラー情報１"               TO CSV2-F04
       WHEN   "3"
              MOVE NC"エラー情報１"               TO CSV2-F04
       WHEN   "4"
              MOVE NC"エラー情報１"               TO CSV2-F04
       WHEN   OTHER
              MOVE NC"　　　　　　"               TO CSV2-F04
     END-EVALUATE.
     MOVE     X"29"                               TO CSV2-FE04.
     MOVE     ALL ","                             TO CSV2-FK04.
*
     MOVE    X"28"                                TO CSV2-FS05.
     EVALUATE PARA-IN-JOBKIND
       WHEN   "1"
              MOVE NC"　　　　　　"               TO CSV2-F05
       WHEN   "2"
              MOVE NC"エラー情報２"               TO CSV2-F05
       WHEN   "3"
              MOVE NC"エラー情報２"               TO CSV2-F05
       WHEN   "4"
              MOVE NC"エラー情報２"               TO CSV2-F05
       WHEN   OTHER
              MOVE NC"　　　　　　"               TO CSV2-F05
     END-EVALUATE.
     MOVE     X"29"                               TO CSV2-FE05.
     MOVE     ALL ","                             TO CSV2-FK05.
*
     MOVE    X"28"                                TO CSV2-FS06.
     EVALUATE PARA-IN-JOBKIND
       WHEN   "1"
              MOVE NC"　　　　　　"               TO CSV2-F06
       WHEN   "2"
              MOVE NC"エラー情報３"               TO CSV2-F06
       WHEN   "3"
              MOVE NC"エラー情報３"               TO CSV2-F06
       WHEN   "4"
              MOVE NC"エラー情報３"               TO CSV2-F06
       WHEN   OTHER
              MOVE NC"　　　　　　"               TO CSV2-F06
     END-EVALUATE.
     MOVE     X"29"                               TO CSV2-FE06.
     MOVE     ALL ","                             TO CSV2-FK06.
*
     MOVE    X"28"                                TO CSV2-FS07.
     EVALUATE PARA-IN-JOBKIND
       WHEN   "1"
              MOVE NC"　　　　　　"               TO CSV2-F07
       WHEN   "2"
              MOVE NC"エラー情報４"               TO CSV2-F07
       WHEN   "3"
              MOVE NC"エラー情報４"               TO CSV2-F07
       WHEN   "4"
              MOVE NC"エラー情報４"               TO CSV2-F07
       WHEN   OTHER
              MOVE NC"　　　　　　"               TO CSV2-F07
     END-EVALUATE.
     MOVE     X"29"                               TO CSV2-FE07.
     MOVE     ALL ","                             TO CSV2-FK07.
*
     MOVE    X"28"                                TO CSV2-FS08.
     EVALUATE PARA-IN-JOBKIND
       WHEN   "1"
              MOVE NC"　　　　　　"               TO CSV2-F08
       WHEN   "2"
              MOVE NC"エラー情報５"               TO CSV2-F08
       WHEN   "3"
              MOVE NC"エラー情報５"               TO CSV2-F08
       WHEN   "4"
              MOVE NC"エラー情報５"               TO CSV2-F08
       WHEN   OTHER
              MOVE NC"　　　　　　"               TO CSV2-F08
     END-EVALUATE.
     MOVE     X"29"                               TO CSV2-FE08.
     MOVE     ALL ","                             TO CSV2-FK08.
*
 AMZCHKC2-SET-EXIT.
     EXIT.
****************************************************************
*出荷確認ＣＳＶ   SET   3.項目タイトルレコード２
****************************************************************
 AMZCHKC3-SET-SEC    SECTION.
*
     MOVE    "AMZCHKC3-SET-SEC"                   TO S-NAME.
*
     MOVE     SPACE                               TO CSV3-REC.
*
     MOVE    X"28"                                TO CSV3-KS01.
     MOVE    NC"バッチ（日）"                     TO CSV3-K01.
     MOVE    X"29"                                TO CSV3-KE01.
     MOVE    ","                                  TO CSV3-KK01.
*
     MOVE    X"28"                                TO CSV3-KS02.
     MOVE    NC"バッチ（時）"                     TO CSV3-K02.
     MOVE    X"29"                                TO CSV3-KE02.
     MOVE    ","                                  TO CSV3-KK02.
*
     MOVE    X"28"                                TO CSV3-KS03.
     MOVE    NC"バッチ（取）"                     TO CSV3-K03.
     MOVE    X"29"                                TO CSV3-KE03.
     MOVE    ","                                  TO CSV3-KK03.
*
     MOVE    X"28"                                TO CSV3-KS04.
     MOVE    NC"処理日付"                         TO CSV3-K04.
     MOVE    X"29"                                TO CSV3-KE04.
     MOVE    ","                                  TO CSV3-KK04.
*
     MOVE    X"28"                                TO CSV3-KS05.
     MOVE    NC"処理時刻"                         TO CSV3-K05.
     MOVE    X"29"                                TO CSV3-KE05.
     MOVE    ","                                  TO CSV3-KK05.
*
     MOVE    X"28"                                TO CSV3-KS06.
     MOVE    NC"担当者部門"                       TO CSV3-K06.
     MOVE    X"29"                                TO CSV3-KE06.
     MOVE    ","                                  TO CSV3-KK06.
*
     MOVE    X"28"                                TO CSV3-KS07.
     MOVE    NC"担当者"                           TO CSV3-K07.
     MOVE    X"29"                                TO CSV3-KE07.
     MOVE    ","                                  TO CSV3-KK07.
*
     MOVE    X"28"                                TO CSV3-KS08.
     MOVE    NC"発注書番号"                       TO CSV3-K08.
     MOVE    X"29"                                TO CSV3-KE08.
     MOVE    ","                                  TO CSV3-KK08.
*
     MOVE    X"28"                                TO CSV3-KS09.
     MOVE    NC"発注日付"                         TO CSV3-K09.
     MOVE    X"29"                                TO CSV3-KE09.
     MOVE    ","                                  TO CSV3-KK09.
*
     MOVE    X"28"                                TO CSV3-KS10.
     MOVE    NC"ＩＤコード"                       TO CSV3-K10.
     MOVE    X"29"                                TO CSV3-KE10.
     MOVE    ","                                  TO CSV3-KK10.
*
     MOVE    X"28"                                TO CSV3-KS11.
     MOVE    NC"明細行番号"                       TO CSV3-K11.
     MOVE    X"29"                                TO CSV3-KE11.
     MOVE    ","                                  TO CSV3-KK11.
*
     MOVE    X"28"                                TO CSV3-KS12.
     MOVE    NC"発注数量"                         TO CSV3-K12.
     MOVE    X"29"                                TO CSV3-KE12.
     MOVE    ","                                  TO CSV3-KK12.
*
     MOVE    X"28"                                TO CSV3-KS13.
     MOVE    NC"本体価格"                         TO CSV3-K13.
     MOVE    X"29"                                TO CSV3-KE13.
     MOVE    ","                                  TO CSV3-KK13.
*
     MOVE    X"28"                                TO CSV3-KS14.
     MOVE    NC"単価コード"                       TO CSV3-K14.
     MOVE    X"29"                                TO CSV3-KE14.
     MOVE    ","                                  TO CSV3-KK14.
*
     MOVE    X"28"                                TO CSV3-KS15.
     MOVE    NC"商品種別コード"                   TO CSV3-K15.
     MOVE    X"29"                                TO CSV3-KE15.
     MOVE    ","                                  TO CSV3-KK15.
*
     MOVE    X"28"                                TO CSV3-KS16.
     MOVE    NC"品番"                             TO CSV3-K16.
     MOVE    X"29"                                TO CSV3-KE16.
     MOVE    ","                                  TO CSV3-KK16.
*
     MOVE    X"28"                                TO CSV3-KS17.
     MOVE    NC"連携得意先コード"                 TO CSV3-K17.
     MOVE    X"29"                                TO CSV3-KE17.
     MOVE    ","                                  TO CSV3-KK17.
*
     MOVE    X"28"                                TO CSV3-KS18.
     MOVE    NC"連携データ種別コード"             TO CSV3-K18.
     MOVE    X"29"                                TO CSV3-KE18.
     MOVE    ","                                  TO CSV3-KK18.
*
     MOVE    X"28"                                TO CSV3-KS19.
     MOVE    NC"連携受信日付時刻"                 TO CSV3-K19.
     MOVE    X"29"                                TO CSV3-KE19.
     MOVE    ","                                  TO CSV3-KK19.
*
     MOVE    X"28"                                TO CSV3-KS20.
     MOVE    NC"連携ＳＥＱ"                       TO CSV3-K20.
     MOVE    X"29"                                TO CSV3-KE20.
     MOVE    ","                                  TO CSV3-KK20.
*
     MOVE    X"28"                                TO CSV3-KS21.
     MOVE    NC"連携ＳＥＱ２"                     TO CSV3-K21.
     MOVE    X"29"                                TO CSV3-KE21.
     MOVE    ","                                  TO CSV3-KK21.
*
     MOVE    X"28"                                TO CSV3-KS22.
     MOVE    NC"連携ＳＥＱ３"                     TO CSV3-K22.
     MOVE    X"29"                                TO CSV3-KE22.
     MOVE    ","                                  TO CSV3-KK22.
*
     MOVE    X"28"                                TO CSV3-KS23.
     MOVE    NC"処理日付"                         TO CSV3-K23.
     MOVE    X"29"                                TO CSV3-KE23.
     MOVE    ","                                  TO CSV3-KK23.
*
     MOVE    X"28"                                TO CSV3-KS24.
     MOVE    NC"処理時刻"                         TO CSV3-K24.
     MOVE    X"29"                                TO CSV3-KE24.
     MOVE    ","                                  TO CSV3-KK24.
*
     MOVE    X"28"                                TO CSV3-KS25.
     MOVE    NC"担当者部門"                       TO CSV3-K25.
     MOVE    X"29"                                TO CSV3-KE25.
     MOVE    ","                                  TO CSV3-KK25.
*
     MOVE    X"28"                                TO CSV3-KS26.
     MOVE    NC"担当者"                           TO CSV3-K26.
     MOVE    X"29"                                TO CSV3-KE26.
     MOVE    ","                                  TO CSV3-KK26.
*
     MOVE    X"28"                                TO CSV3-KS27.
     MOVE    NC"基幹＿伝票番号"                   TO CSV3-K27.
     MOVE    X"29"                                TO CSV3-KE27.
     MOVE    ","                                  TO CSV3-KK27.
*
     MOVE    X"28"                                TO CSV3-KS28.
     MOVE    NC"基幹＿行番号"                     TO CSV3-K28.
     MOVE    X"29"                                TO CSV3-KE28.
     MOVE    ","                                  TO CSV3-KK28.
*
     MOVE    X"28"                                TO CSV3-KS29.
     MOVE    NC"状態区分"                         TO CSV3-K29.
     MOVE    X"29"                                TO CSV3-KE29.
     MOVE    ","                                  TO CSV3-KK29.
*
     MOVE    X"28"                                TO CSV3-KS30.
     MOVE    NC"出荷予定数"                       TO CSV3-K30.
     MOVE    X"29"                                TO CSV3-KE30.
     MOVE    ","                                  TO CSV3-KK30.
*
     MOVE    X"28"                                TO CSV3-KS31.
     MOVE    NC"消費税率"                         TO CSV3-K31.
     MOVE    X"29"                                TO CSV3-KE31.
     MOVE    ","                                  TO CSV3-KK31.
*
     MOVE    X"28"                                TO CSV3-KS32.
     MOVE    NC"倉庫コード"                       TO CSV3-K32.
     MOVE    X"29"                                TO CSV3-KE32.
     MOVE    ","                                  TO CSV3-KK32.
*
     MOVE    X"28"                                TO CSV3-KS33.
     MOVE    NC"ロケーションコード"               TO CSV3-K33.
     MOVE    X"29"                                TO CSV3-KE33.
     MOVE    ","                                  TO CSV3-KK33.
*
     MOVE    X"28"                                TO CSV3-KS34.
     MOVE    NC"店番"                             TO CSV3-K34.
     MOVE    X"29"                                TO CSV3-KE34.
     MOVE    ","                                  TO CSV3-KK34.
*
     MOVE    X"28"                                TO CSV3-KS35.
     MOVE    NC"ケース入数"                       TO CSV3-K35.
     MOVE    X"29"                                TO CSV3-KE35.
     MOVE    ","                                  TO CSV3-KK35.
*
     MOVE    X"28"                                TO CSV3-KS36.
     MOVE    NC"処理日付"                         TO CSV3-K36.
     MOVE    X"29"                                TO CSV3-KE36.
     MOVE    ","                                  TO CSV3-KK36.
*
     MOVE    X"28"                                TO CSV3-KS37.
     MOVE    NC"処理時刻"                         TO CSV3-K37.
     MOVE    X"29"                                TO CSV3-KE37.
     MOVE    ","                                  TO CSV3-KK37.
*
     MOVE    X"28"                                TO CSV3-KS38.
     MOVE    NC"担当者部門"                       TO CSV3-K38.
     MOVE    X"29"                                TO CSV3-KE38.
     MOVE    ","                                  TO CSV3-KK38.
*
     MOVE    X"28"                                TO CSV3-KS39.
     MOVE    NC"担当者"                           TO CSV3-K39.
     MOVE    X"29"                                TO CSV3-KE39.
     MOVE    ","                                  TO CSV3-KK39.
*
     MOVE    X"28"                                TO CSV3-KS40.
     MOVE    NC"梱包数"                           TO CSV3-K40.
     MOVE    X"29"                                TO CSV3-KE40.
     MOVE    ","                                  TO CSV3-KK40.
*
     MOVE    X"28"                                TO CSV3-KS41.
     MOVE    NC"重量"                             TO CSV3-K41.
     MOVE    X"29"                                TO CSV3-KE41.
     MOVE    ","                                  TO CSV3-KK41.
*
     MOVE    X"28"                                TO CSV3-KS42.
     MOVE    NC"出荷日"                           TO CSV3-K42.
     MOVE    X"29"                                TO CSV3-KE42.
     MOVE    ","                                  TO CSV3-KK42.
*
     MOVE    X"28"                                TO CSV3-KS43.
     MOVE    NC"ＳＳＣＣコード"                   TO CSV3-K43.
     MOVE    X"29"                                TO CSV3-KE43.
     MOVE    ","                                  TO CSV3-KK43.
*
     MOVE    X"28"                                TO CSV3-KS44.
     MOVE    NC"出荷検品数量"                     TO CSV3-K44.
     MOVE    X"29"                                TO CSV3-KE44.
     MOVE    ","                                  TO CSV3-KK44.
*
     MOVE    X"28"                                TO CSV3-KS45.
     MOVE    NC"納品日"                           TO CSV3-K45.
     MOVE    X"29"                                TO CSV3-KE45.
     MOVE    ","                                  TO CSV3-KK45.
*
 AMZCHKC3-SET-EXIT.
     EXIT.
****************************************************************
*出荷確認ＣＳＶ   初期値設定   4.明細行
****************************************************************
 AMZCHKC4-INIT-SEC    SECTION.
*
     MOVE     "AMZCHKC4-INIT-SEC" TO  S-NAME.
*
     MOVE     SPACE          TO   CSV4-REC.
*制御バイト
     MOVE    X"28"           TO   CSV4-MS29
                                  CSV4-MS46
                                  CSV4-MS47
                                  CSV4-MS48
                                  CSV4-MS49
                                  CSV4-MS50.
     MOVE    X"29"           TO   CSV4-ME29
                                  CSV4-ME46
                                  CSV4-ME47
                                  CSV4-ME48
                                  CSV4-ME49
                                  CSV4-ME50.
*カンマ
     MOVE    ","             TO   CSV4-MK01
                                  CSV4-MK02
                                  CSV4-MK03
                                  CSV4-MK04
                                  CSV4-MK05
                                  CSV4-MK06
                                  CSV4-MK07
                                  CSV4-MK08
                                  CSV4-MK09
                                  CSV4-MK10
                                  CSV4-MK11
                                  CSV4-MK12
                                  CSV4-MK13
                                  CSV4-MK14
                                  CSV4-MK15
                                  CSV4-MK16
                                  CSV4-MK17
                                  CSV4-MK18
                                  CSV4-MK19
                                  CSV4-MK20
                                  CSV4-MK21
                                  CSV4-MK22
                                  CSV4-MK23
                                  CSV4-MK24
                                  CSV4-MK25
                                  CSV4-MK26
                                  CSV4-MK27
                                  CSV4-MK28
                                  CSV4-MK29
                                  CSV4-MK30
                                  CSV4-MK31
                                  CSV4-MK32
                                  CSV4-MK33
                                  CSV4-MK34
                                  CSV4-MK35
                                  CSV4-MK36
                                  CSV4-MK37
                                  CSV4-MK38
                                  CSV4-MK39
                                  CSV4-MK40
                                  CSV4-MK41
                                  CSV4-MK42
                                  CSV4-MK43
                                  CSV4-MK44
                                  CSV4-MK45
                                  CSV4-MK46
                                  CSV4-MK47
                                  CSV4-MK48
                                  CSV4-MK49
                                  CSV4-MK50.
*
 AMZCHKC4-INIT-EXIT.
     EXIT.
****************************************************************
*出荷確認ＣＳＶ   SET   4.明細行
****************************************************************
 AMZCHKC4-SET-SEC     SECTION.
*
     MOVE   "AMZCHKC4-SET-SEC"  TO  S-NAME.
*
*バッチ（日付）
     MOVE    CHK-FE01           TO  CSV4-M01.
*バッチ（時刻）
     MOVE    CHK-FE02           TO  CSV4-M02.
*バッチ（取Ｃ）
     MOVE    CHK-FE03           TO  CSV4-M03.
*受注（日付）
     MOVE    CHK-FE04           TO  CSV4-M04.
*受注（時刻）
     MOVE    CHK-FE05           TO  CSV4-M05.
*受注（部Ｃ）
     MOVE    CHK-FE06           TO  CSV4-M06.
*受注（担Ｃ）
     MOVE    CHK-FE07           TO  CSV4-M07.
*発注書番号
     MOVE    CHK-FA03           TO  CSV4-M08.
*発注日付
     MOVE    CHK-FA04           TO  CSV4-M09.
*ＩＤコード
     MOVE    CHK-FA08           TO  CSV4-M10.
*明細行番号
     MOVE    CHK-FA09           TO  CSV4-M11.
*発注数量
     MOVE    CHK-FA10           TO  CSV4-M12.
*本体価格
     MOVE    CHK-FA11           TO  CSV4-M13.
*単価コード
     MOVE    CHK-FA12           TO  CSV4-M14.
*商品種別コード
     MOVE    CHK-FA13           TO  CSV4-M15.
*品番
     MOVE    CHK-FA14           TO  CSV4-M16.
*連携得意先コード
     MOVE    CHK-FA15           TO  CSV4-M17.
*連携データ種別コード
     MOVE    CHK-FA16           TO  CSV4-M18.
*連携受信日付時刻
     MOVE    CHK-FA17           TO  CSV4-M19.
*連携SEQ
     MOVE    CHK-FA18           TO  CSV4-M20.
*連携SEQ2
     MOVE    CHK-FA19           TO  CSV4-M21.
*連携SEQ3
     MOVE    CHK-FA20           TO  CSV4-M22.
*引当（日付）
     MOVE    CHK-FE09           TO  CSV4-M23.
*引当（時刻）
     MOVE    CHK-FE10           TO  CSV4-M24.
*引当（部Ｃ）
     MOVE    CHK-FE11           TO  CSV4-M25.
*引当（担Ｃ）
     MOVE    CHK-FE12           TO  CSV4-M26.
*基幹伝票ＮＯ
     MOVE    CHK-FB01           TO  CSV4-M27.
*基幹行ＮＯ
     MOVE    CHK-FB02           TO  CSV4-M28.
*状態区分
     IF       CHK-F001  NOT = "1"
     EVALUATE CHK-FB03
       WHEN   "AC"
              MOVE NC"ＡＣ：在庫有"               TO CSV4-M29
       WHEN   "OS"
              MOVE NC"ＯＳ：在庫無"               TO CSV4-M29
       WHEN   OTHER
              MOVE NC"？？：？？？"               TO CSV4-M29
     END-EVALUATE
     END-IF.
*出荷予定数量
     MOVE    CHK-FB04           TO  CSV4-M30.
*消費税率
     MOVE    CHK-FB06           TO  CSV4-M31.
*倉庫コード
     MOVE    CHK-FB09           TO  CSV4-M32.
*ロケーションコード
     MOVE    CHK-FB10           TO  CSV4-M33.
*店番
     MOVE    CHK-FB11           TO  CSV4-M34.
*ケース入数
     MOVE    CHK-FB13           TO  CSV4-M35.
*確定（日付）
     MOVE    CHK-FE14           TO  CSV4-M36.
*確定（時刻）
     MOVE    CHK-FE15           TO  CSV4-M37.
*確定（部Ｃ）
     MOVE    CHK-FE16           TO  CSV4-M38.
*確定（担Ｃ）
     MOVE    CHK-FE17           TO  CSV4-M39.
*梱包数
     MOVE    CHK-FC01           TO  CSV4-M40.
*重量
     MOVE    CHK-FC02           TO  CSV4-M41.
*出荷日
     MOVE    CHK-FC03           TO  CSV4-M42.
*SSCCコード番号
     MOVE    CHK-FC04           TO  CSV4-M43.
*出荷商品数量
     MOVE    CHK-FC05           TO  CSV4-M44.
*納品日
     MOVE    CHK-FE18           TO  CSV4-M45.
*エラー情報１
     MOVE     SPACE                               TO CSV4-M46.
     EVALUATE PARA-IN-JOBKIND
       WHEN   "1"
              MOVE NC"　　　　　　　　　　"       TO CSV4-M46
       WHEN   "2"
          IF CHK-FE081  =  "1"
              MOVE NC"受注データ重複　　　"       TO CSV4-M46
          END-IF
       WHEN   "3"
          IF CHK-FE131  =  "1"
              MOVE NC"　　　　　　　　　　"       TO CSV4-M46
          END-IF
       WHEN   "4"
          IF CHK-FE191  =  "1"
              MOVE NC"出荷指示なし　　　　"       TO CSV4-M46
          END-IF
       WHEN   OTHER
              MOVE NC"？？？？？？？？？？"       TO CSV4-M46
     END-EVALUATE.
*エラー情報２
     MOVE     SPACE                               TO CSV4-M47.
     EVALUATE PARA-IN-JOBKIND
       WHEN   "1"
              MOVE NC"　　　　　　　　　　"       TO CSV4-M47
       WHEN   "2"
          IF CHK-FE082  =  "1"
              MOVE NC"店舗コードなし　　　"       TO CSV4-M47
          END-IF
       WHEN   "3"
          IF CHK-FE132  =  "1"
              MOVE NC"　　　　　　　　　　"       TO CSV4-M47
          END-IF
       WHEN   "4"
          IF CHK-FE192  =  "1"
              MOVE NC"指示数＜出荷数　　　"       TO CSV4-M47
          END-IF
       WHEN   OTHER
              MOVE NC"？？？？？？？？？？"       TO CSV4-M47
     END-EVALUATE.
*エラー情報３
     MOVE     SPACE                               TO CSV4-M48.
     EVALUATE PARA-IN-JOBKIND
       WHEN   "1"
              MOVE NC"　　　　　　　　　　"       TO CSV4-M48
       WHEN   "2"
          IF CHK-FE083  =  "1"
              MOVE NC"商品コードなし　　　"       TO CSV4-M48
          END-IF
       WHEN   "3"
          IF CHK-FE133  =  "1"
              MOVE NC"　　　　　　　　　　"       TO CSV4-M48
          END-IF
       WHEN   "4"
          IF CHK-FE193  =  "1"
              MOVE NC"出荷日付異常　　　　"       TO CSV4-M48
          END-IF
       WHEN   OTHER
              MOVE NC"？？？？？？？？？？"       TO CSV4-M48
     END-EVALUATE.
*エラー情報４
     MOVE     SPACE                               TO CSV4-M49.
     EVALUATE PARA-IN-JOBKIND
       WHEN   "1"
              MOVE NC"　　　　　　　　　　"       TO CSV4-M49
       WHEN   "2"
          IF CHK-FE084  =  "1"
              MOVE NC"発注日付異常　　　　"       TO CSV4-M49
          END-IF
       WHEN   "3"
          IF CHK-FE134  =  "1"
              MOVE NC"　　　　　　　　　　"       TO CSV4-M49
          END-IF
       WHEN   "4"
          IF CHK-FE194  =  "1"
              MOVE NC"出荷取込・更新済　　"       TO CSV4-M49
          END-IF
       WHEN   OTHER
              MOVE NC"？？？？？？？？？？"       TO CSV4-M48
     END-EVALUATE.
*エラー情報５
     MOVE     SPACE                               TO CSV4-M50.
     EVALUATE PARA-IN-JOBKIND
       WHEN   "1"
              MOVE NC"　　　　　　　　　　"       TO CSV4-M50
       WHEN   "2"
          IF CHK-FE085  =  "1"
              MOVE NC"　　　　　　　　　　"       TO CSV4-M50
          END-IF
       WHEN   "3"
          IF CHK-FE135  =  "1"
              MOVE NC"　　　　　　　　　　"       TO CSV4-M50
          END-IF
       WHEN   "4"
          IF CHK-FE195  =  "1"
              MOVE NC"　　　　　　　　　　"       TO CSV4-M50
          END-IF
       WHEN   OTHER
              MOVE NC"？？？？？？？？？？"       TO CSV4-M48
     END-EVALUATE.
*
 AMZCHKC4-SET-EXIT.
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
     CLOSE    AMZCHKW1   AMZCHKCS.
*
     DISPLAY  MSG-END UPON CONS.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
