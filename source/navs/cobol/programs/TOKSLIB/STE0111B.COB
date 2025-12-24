****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　手書発注業務                      *
*    モジュール名　　　　：　発注集計表データ抽出（手書）      *
*    作成日／更新日　　　：　99/12/07                          *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    作成日／更新日　　　：　05/09/12                          *
*    作成者／更新者　　　：　ＮＡＶ松野　　　　　　　　　　　　*
*    処理概要　　　　　　：　受け取った各パラメタより、該当    *
*                            のデータを売上伝票データファイル  *
*                            より抽出する。                    *
*                            05/09/12    税込み売価修正        *
*    更新履歴      ：
*     2012/02/23 三浦/NAV 基幹サーバ統合
*    作成日／更新日　　　：　2013/12/11 *$$                    *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　消費税増税対応　　　　　　　　　  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            STE0111B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          99/12/07.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票データ
     SELECT   JHTDENF  ASSIGN    TO        DA-01-VI-JHTDENF
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
                                            DEN-F07   DEN-F112
                                            DEN-F03
                        FILE  STATUS   IS   DEN-STATUS.
*発注集計表ワーク
     SELECT   SHWHACF   ASSIGN    TO        DA-01-S-SHWHACF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   HAC-STATUS.
*取引先マスタ
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
*商品コード変換テーブル（取引先ＣＤ＋出荷場所＋自社＋品単）
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F01
                                                 SHO-F04
                                                 SHO-F031
                                                 SHO-F032
                        FILE      STATUS    IS   SHO-STATUS.
*商品コード変換テーブル（取引先ＣＤ＋出荷場所＋量販店商品）
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SH1-F01
                                                 SH1-F02
                        FILE      STATUS    IS   SH1-STATUS.
*----<2005/09/12 追加>----
*条件ファイル
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01  JYO-F02
                        FILE      STATUS    IS    JYO-STATUS.
*-------------------------
*
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  JHTDENF
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    発注集計表データファイル
******************************************************************
 FD  SHWHACF            BLOCK     CONTAINS  5    RECORDS.
     COPY     SHWHACF   OF        XFDLIB
              JOINING   HAC       PREFIX.
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
******************************************************************
*    商品変換テーブル_
******************************************************************
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
******************************************************************
*    商品変換テーブル_
******************************************************************
 FD  SHOTBL1.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SH1       PREFIX.
******************************************************************
*    条件ファイル(2005/09/12 追加)
******************************************************************
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
*
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  HAC-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  SHO-STATUS        PIC  X(02).
     03  SH1-STATUS        PIC  X(02).
*----<2005/09/12 追加>----
     03  JYO-STATUS        PIC  X(02).
*-------------------------
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "STE0111B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "STE0111B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "STE0111B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*----<2005/09/12 追加>----
*税計算用
*$$2013/12/11 NAV ST 消費税増税対応
 01  WK-ZEIRITU             PIC   9(01)V99  VALUE  ZERO.
 01  WK-ZEIRITU1            PIC   9(01)V99  VALUE  ZERO.
 01  WK-ZEIRITU2            PIC   9(01)V99  VALUE  ZERO.
 01  WK-ZEIKAITEI           PIC   9(08)     VALUE  ZERO.
 01  WK-ZEIRITU3            PIC   9(01)V99  VALUE  ZERO.
 01  WK-ZEIRITU4            PIC   9(01)V99  VALUE  ZERO.
*$$2013/12/11 NAV ED 消費税増税対応
*-------------------------
*
 LINKAGE                SECTION.
 01  PARA-SOKCD             PIC   X(02).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SDENP             PIC   9(09).
 01  PARA-EDENP             PIC   9(09).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-SOKCD
                                       PARA-TORICD
                                       PARA-SDENP
                                       PARA-EDENP.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHTDENF.
     MOVE      "JHTDENF"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHWHACF.
     MOVE      "SHWHACF "   TO   AB-FILE.
     MOVE      HAC-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TOKMS2.
     MOVE      "TOKMS2"     TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HSHOTBL.
     MOVE      "SHOTBL2"    TO   AB-FILE.
     MOVE      SHO-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL1.
     MOVE      "SHOTBL1"    TO   AB-FILE.
     MOVE      SH1-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*----<2005/09/12 追加>----
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HJYOKEN.
     MOVE      "HJYOKEN"    TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*-------------------------
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     JHTDENF TOKMS2 HSHOTBL SHOTBL1 HJYOKEN.
     OPEN     OUTPUT    SHWHACF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
**** DISPLAY  "SOKCD  = " PARA-SOKCD  UPON CONS.
**** DISPLAY  "TORICD = " PARA-TORICD UPON CONS.
**** DISPLAY  "SDENP  = " PARA-SDENP  UPON CONS.
**** DISPLAY  "EDENP  = " PARA-EDENP  UPON CONS.
*    得意先マスタ検索
     MOVE     SPACE          TO   TOK-REC
     INITIALIZE                   TOK-REC
     MOVE     PARA-TORICD    TO   TOK-F01
     READ     TOKMS2
         INVALID
              MOVE SPACE     TO   TOK-REC
              INITIALIZE          TOK-REC
     END-READ.
*----<2005/09/12 追加>----
*    税率の取得
     MOVE     SPACE          TO   JYO-REC.
     INITIALIZE                   JYO-REC.
     MOVE     "99"           TO   JYO-F01.
     MOVE     "ZEI"          TO   JYO-F02.
     READ     HJYOKEN
*$$2013/12/11 NAV ST  消費税増税対応
*                     消費税税率が取得出来ない場合は異常終了
********INVALID
**************MOVE  ZERO     TO   WK-ZEIRITU1
        INVALID
              DISPLAY NC"消費税率取得エラー！！" UPON CONS
              MOVE 4000      TO   PROGRAM-STATUS
              STOP  RUN
********NOT INVALID
**************MOVE  JYO-F04  TO   WK-ZEIRITU1
        NOT INVALID
*                     項目変更
              MOVE JYO-F04   TO   WK-ZEIRITU1
              MOVE JYO-F05   TO   WK-ZEIRITU2
              MOVE JYO-F06   TO   WK-ZEIKAITEI
              MOVE JYO-F07   TO   WK-ZEIRITU3
              MOVE JYO-F08   TO   WK-ZEIRITU4
*$$2013/12/11 NAV ED  消費税増税対応
     END-READ.
*****DISPLAY  "税率" WK-ZEIRITU1 UPON CONS.
*-------------------------
*
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     PARA-TORICD    TO   DEN-F01.
     MOVE     PARA-SDENP     TO   DEN-F02.
     MOVE     ZERO           TO   DEN-F04.
     MOVE     ZERO           TO   DEN-F051.
     MOVE     ZERO           TO   DEN-F07.
     MOVE     ZERO           TO   DEN-F112.
     MOVE     ZERO           TO   DEN-F03.
     START    JHTDENF  KEY  >=    DEN-F01   DEN-F02
                                  DEN-F04   DEN-F051
                                  DEN-F07 DEN-F112 DEN-F03
         INVALID   KEY
              MOVE      9    TO   END-FG
              GO   TO   INIT-EXIT
     END-START.
*
 INIT-010.
*
     PERFORM  JHTDENF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
     IF     ( PARA-TORICD    =    DEN-F01 ) AND
            ( PARA-SDENP     <=   DEN-F02 ) AND
            ( PARA-EDENP     >=   DEN-F02 )
              CONTINUE
     ELSE
              MOVE      9         TO   END-FG
              GO        TO        MAIN-EXIT
     END-IF.
*
*出荷明細ワーク出力
     MOVE     SPACE          TO   HAC-REC.
     INITIALIZE                   HAC-REC.
     MOVE     DEN-F01        TO   HAC-F013.
     MOVE     DEN-F07        TO   HAC-F02.
     MOVE     DEN-F48        TO   HAC-F03.
*_番取得（商品変換テーブル）
     IF       DEN-F25   NOT   =    SPACE
              MOVE    DEN-F01         TO   SH1-F01
              MOVE    DEN-F25         TO   SH1-F02
              READ    SHOTBL1
                      INVALID   KEY
                      MOVE    SPACE   TO   HAC-F09
                      NOT INVALID    KEY
                      MOVE    SH1-F08 TO   HAC-F09
                      GO              TO   MAIN010
             END-READ
     END-IF.
*
     MOVE    DEN-F01         TO   SHO-F01.
     MOVE    DEN-F08         TO   SHO-F04
     MOVE    DEN-F1411       TO   SHO-F031
     MOVE    DEN-F1412       TO   SHO-F032
     READ    HSHOTBL
           INVALID   KEY
             MOVE    SPACE   TO   HAC-F09
           NOT INVALID    KEY
             MOVE    SHO-F08 TO   HAC-F09
     END-READ.
 MAIN010.
     MOVE     DEN-F12        TO   HAC-F04.
     MOVE     DEN-F111       TO   HAC-F05.
     MOVE     DEN-F112       TO   HAC-F06.
     MOVE     DEN-F25        TO   HAC-F07.
     MOVE     DEN-F1411      TO   HAC-F081.
     MOVE     DEN-F1412      TO   HAC-F082.
     MOVE     SPACE          TO   HAC-F10.
     MOVE     DEN-F1421      TO   HAC-F111.
     MOVE     DEN-F1422      TO   HAC-F112.
     MOVE     SPACE          TO   HAC-F12.
     MOVE     DEN-F15        TO   HAC-F13.
     MOVE     DEN-F15        TO   HAC-F14.
     MOVE     DEN-F172       TO   HAC-F15.
     MOVE     DEN-F173       TO   HAC-F16.
     MOVE     DEN-F181       TO   HAC-F18.
     MOVE     DEN-F182       TO   HAC-F19.
*----<2005/09/12 追加>----
*$$2013/12/11 NAV ST 消費税増税対応
     IF       TOK-F97       =    "0"
**************EVALUATE      TOK-F88
*                 WHEN      "0"
*                 COMPUTE   HAC-F21 = HAC-F16 * WK-ZEIRITU1
*                 WHEN      "4"
*                 COMPUTE   HAC-F21 ROUNDED = HAC-F16 * WK-ZEIRITU
*                 WHEN      "9"
*                 COMPUTE   HAC-F21 = HAC-F16 * WK-ZEIRITU1 + 0.99
*                 WHEN      OTHER
*                 MOVE      HAC-F16     TO        HAC-F21
**************END-EVALUATE
      IF  WK-ZEIKAITEI NOT = ZERO    *>改訂日が０以上
       IF WK-ZEIKAITEI <= DEN-F112   *>納日が改訂日以上、新税率
          EVALUATE      TOK-F88
              WHEN      "0"
              COMPUTE   HAC-F21 = HAC-F16 * WK-ZEIRITU3
              WHEN      "4"
              COMPUTE   HAC-F21 ROUNDED = HAC-F16 * WK-ZEIRITU3
              WHEN      "9"
              COMPUTE   HAC-F21 = HAC-F16 * WK-ZEIRITU3 + 0.99
              WHEN      OTHER
              MOVE      HAC-F16     TO        HAC-F21
          END-EVALUATE
       ELSE                          *>納日が改訂日以下、旧税率
          EVALUATE      TOK-F88
              WHEN      "0"
              COMPUTE   HAC-F21 = HAC-F16 * WK-ZEIRITU1
              WHEN      "4"
              COMPUTE   HAC-F21 ROUNDED = HAC-F16 * WK-ZEIRITU1
              WHEN      "9"
              COMPUTE   HAC-F21 = HAC-F16 * WK-ZEIRITU1 + 0.99
              WHEN      OTHER
              MOVE      HAC-F16     TO        HAC-F21
          END-EVALUATE
       END-IF
      ELSE
          EVALUATE      TOK-F88      *>改訂日が０の場合、旧税率
              WHEN      "0"
              COMPUTE   HAC-F21 = HAC-F16 * WK-ZEIRITU1
              WHEN      "4"
              COMPUTE   HAC-F21 ROUNDED = HAC-F16 * WK-ZEIRITU1
              WHEN      "9"
              COMPUTE   HAC-F21 = HAC-F16 * WK-ZEIRITU1 + 0.99
              WHEN      OTHER
              MOVE      HAC-F16     TO        HAC-F21
          END-EVALUATE
      END-IF
*$$2013/12/11 NAV ED 消費税増税対応
     ELSE
              MOVE           HAC-F16    TO        HAC-F21
     END-IF.
*****DISPLAY  "TOK-F97" TOK-F97    UPON CONS.
*****DISPLAY  "税区分"  TOK-F88    UPON CONS.
*****DISPLAY  "税抜き"  HAC-F16    UPON CONS.
*****DISPLAY  "税込み"  HAC-F21    UPON CONS.
*-------------------------
     MOVE     TOK-F04        TO   HAC-F17.
     WRITE    HAC-REC.
     ADD      1              TO   WRT-CNT.
*
     PERFORM  JHTDENF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     JHTDENF TOKMS2 SHWHACF HSHOTBL SHOTBL1 HJYOKEN.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　売上ファイル読込み処理　　　　　　　　　　　　　*
****************************************************************
 JHTDENF-READ-SEC      SECTION.
*
     MOVE     "JHTDENF-READ-SEC"  TO   S-NAME.
*
     READ     JHTDENF
              AT END
                   MOVE     9     TO   END-FG
                   GO             TO   JHTDENF-READ-EXIT
              NOT AT END
                   ADD      1     TO   RD-CNT
     END-READ.
*
     IF       DEN-F03  >=  80
              GO                  TO   JHTDENF-READ-SEC
     END-IF.
*
 JHTDENF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
