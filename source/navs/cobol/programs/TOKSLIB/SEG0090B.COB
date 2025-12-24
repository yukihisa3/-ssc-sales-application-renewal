****************************************************************
*    （営業所－データ抽出バージョン）                          *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　営業所データ連携                  *
*    業務名　　　　　　　：　営業所データ連携                 *
*    モジュール名　　　　：　手書伝票データ抽出処理            *
*    作成日／更新日　　　：　99/09/13                          *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　受け取ったパラメタの倉庫コード    *
*                            抽出取引先、伝票範囲より該当の    *
*                            データを抽出する。                *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SEG0090B.
 AUTHOR.                NAV Y.YOSHIDA.
 DATE-WRITTEN.          00/03/30.
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
     SELECT   SHTDENLC  ASSIGN    TO        DA-01-VI-SHTDENLC
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F277  DEN-F274
                                            DEN-F09   DEN-F01
                                            DEN-F02   DEN-F03
                        FILE  STATUS   IS   DEN-STATUS.
*送信用伝票データ
     SELECT   JHTDENF   ASSIGN    TO        JHTDENF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SDE-STATUS.
*店舗マスタ
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52
                                            TEN-F011
                        FILE      STATUS    TEN-STATUS.
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
*
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENLC
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    送信用売上伝票データ
******************************************************************
 FD  JHTDENF            LABEL RECORD   IS   STANDARD.
     COPY     JHTDENF   OF        XFDLIB
              JOINING   SDE       PREFIX.
*
*01  SDE-REC.
*    03       SDE-F01             PIC  9(008).
*    03       FILLER              PIC  X(024).
*    03       SDE-F02  07         PIC  9(005).
*    03       FILLER              PIC  X(193).
*    03       SDE-F03  27F        PIC  9(001).
*    03       FILLER              PIC  X(789).
*
******************************************************************
*    店舗マスタ
******************************************************************
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
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
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  IXA                     PIC  9(02)     VALUE  ZERO.
*
 01  WK-PARA-AREA.
     03  WK-PARA             OCCURS  12.
       05  WK-TORICD         PIC  9(08).
       05  WK-SDENNO         PIC  9(09).
       05  WK-EDENNO         PIC  9(09).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE            PIC 9(06).
     03  SYS-DATEW           PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  SDE-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  SHO-STATUS        PIC  X(02).
     03  SH1-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SEG0090B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SEG0090B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SEG0090B".
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
*
 LINKAGE                SECTION.
 01  PARA-SOKO              PIC   9(02).
 01  PARA-TORICD01          PIC   9(08).
 01  PARA-SDENNO01          PIC   9(09).
 01  PARA-EDENNO01          PIC   9(09).
 01  PARA-TORICD02          PIC   9(08).
 01  PARA-SDENNO02          PIC   9(09).
 01  PARA-EDENNO02          PIC   9(09).
 01  PARA-TORICD03          PIC   9(08).
 01  PARA-SDENNO03          PIC   9(09).
 01  PARA-EDENNO03          PIC   9(09).
 01  PARA-TORICD04          PIC   9(08).
 01  PARA-SDENNO04          PIC   9(09).
 01  PARA-EDENNO04          PIC   9(09).
 01  PARA-TORICD05          PIC   9(08).
 01  PARA-SDENNO05          PIC   9(09).
 01  PARA-EDENNO05          PIC   9(09).
 01  PARA-TORICD06          PIC   9(08).
 01  PARA-SDENNO06          PIC   9(09).
 01  PARA-EDENNO06          PIC   9(09).
 01  PARA-TORICD07          PIC   9(08).
 01  PARA-SDENNO07          PIC   9(09).
 01  PARA-EDENNO07          PIC   9(09).
 01  PARA-TORICD08          PIC   9(08).
 01  PARA-SDENNO08          PIC   9(09).
 01  PARA-EDENNO08          PIC   9(09).
 01  PARA-TORICD09          PIC   9(08).
 01  PARA-SDENNO09          PIC   9(09).
 01  PARA-EDENNO09          PIC   9(09).
 01  PARA-TORICD10          PIC   9(08).
 01  PARA-SDENNO10          PIC   9(09).
 01  PARA-EDENNO10          PIC   9(09).
 01  PARA-TORICD11          PIC   9(08).
 01  PARA-SDENNO11          PIC   9(09).
 01  PARA-EDENNO11          PIC   9(09).
 01  PARA-TORICD12          PIC   9(08).
 01  PARA-SDENNO12          PIC   9(09).
 01  PARA-EDENNO12          PIC   9(09).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION  USING      PARA-SOKO
               PARA-TORICD01  PARA-SDENNO01  PARA-EDENNO01
               PARA-TORICD02  PARA-SDENNO02  PARA-EDENNO02
               PARA-TORICD03  PARA-SDENNO03  PARA-EDENNO03
               PARA-TORICD04  PARA-SDENNO04  PARA-EDENNO04
               PARA-TORICD05  PARA-SDENNO05  PARA-EDENNO05
               PARA-TORICD06  PARA-SDENNO06  PARA-EDENNO06
               PARA-TORICD07  PARA-SDENNO07  PARA-EDENNO07
               PARA-TORICD08  PARA-SDENNO08  PARA-EDENNO08
               PARA-TORICD09  PARA-SDENNO09  PARA-EDENNO09
               PARA-TORICD10  PARA-SDENNO10  PARA-EDENNO10
               PARA-TORICD11  PARA-SDENNO11  PARA-EDENNO11
               PARA-TORICD12  PARA-SDENNO12  PARA-EDENNO12.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENLC.
     MOVE      "SHTDENLC"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHTDENF.
     MOVE      "JHTDENF "   TO   AB-FILE.
     MOVE      SDE-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTENMS.
     MOVE      "HTENMS  "   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HSHOTBL.
     MOVE      "HSHOTBL "   TO   AB-FILE.
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
     MOVE      "SHOTBL1 "   TO   AB-FILE.
     MOVE      SH1-STATUS   TO   AB-STS.
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
     OPEN     INPUT     SHTDENLC  HTENMS  HSHOTBL  SHOTBL1.
     OPEN     OUTPUT    JHTDENF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
*    パラメタ退避
     MOVE     SPACE               TO   WK-PARA-AREA.
     INITIALIZE                        WK-PARA-AREA.
     MOVE     PARA-TORICD01       TO   WK-TORICD (1).
     MOVE     PARA-SDENNO01       TO   WK-SDENNO (1).
     MOVE     PARA-EDENNO01       TO   WK-EDENNO (1).
     MOVE     PARA-TORICD02       TO   WK-TORICD (2).
     MOVE     PARA-SDENNO02       TO   WK-SDENNO (2).
     MOVE     PARA-EDENNO02       TO   WK-EDENNO (2).
     MOVE     PARA-TORICD03       TO   WK-TORICD (3).
     MOVE     PARA-SDENNO03       TO   WK-SDENNO (3).
     MOVE     PARA-EDENNO03       TO   WK-EDENNO (3).
     MOVE     PARA-TORICD04       TO   WK-TORICD (4).
     MOVE     PARA-SDENNO04       TO   WK-SDENNO (4).
     MOVE     PARA-EDENNO04       TO   WK-EDENNO (4).
     MOVE     PARA-TORICD05       TO   WK-TORICD (5).
     MOVE     PARA-SDENNO05       TO   WK-SDENNO (5).
     MOVE     PARA-EDENNO05       TO   WK-EDENNO (5).
     MOVE     PARA-TORICD06       TO   WK-TORICD (6).
     MOVE     PARA-SDENNO06       TO   WK-SDENNO (6).
     MOVE     PARA-EDENNO06       TO   WK-EDENNO (6).
     MOVE     PARA-TORICD07       TO   WK-TORICD (7).
     MOVE     PARA-SDENNO07       TO   WK-SDENNO (7).
     MOVE     PARA-EDENNO07       TO   WK-EDENNO (7).
     MOVE     PARA-TORICD08       TO   WK-TORICD (8).
     MOVE     PARA-SDENNO08       TO   WK-SDENNO (8).
     MOVE     PARA-EDENNO08       TO   WK-EDENNO (8).
     MOVE     PARA-TORICD09       TO   WK-TORICD (9).
     MOVE     PARA-SDENNO09       TO   WK-SDENNO (9).
     MOVE     PARA-EDENNO09       TO   WK-EDENNO (9).
     MOVE     PARA-TORICD10       TO   WK-TORICD(10).
     MOVE     PARA-SDENNO10       TO   WK-SDENNO(10).
     MOVE     PARA-EDENNO10       TO   WK-EDENNO(10).
     MOVE     PARA-TORICD11       TO   WK-TORICD(11).
     MOVE     PARA-SDENNO11       TO   WK-SDENNO(11).
     MOVE     PARA-EDENNO11       TO   WK-EDENNO(11).
     MOVE     PARA-TORICD12       TO   WK-TORICD(12).
     MOVE     PARA-SDENNO12       TO   WK-SDENNO(12).
     MOVE     PARA-EDENNO12       TO   WK-EDENNO(12).
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
     PERFORM  VARYING   IXA  FROM      1    BY   1
              UNTIL     IXA  >    12
         IF   WK-TORICD(IXA)      NOT =     ZERO
*
              MOVE      SPACE          TO   DEN-REC
              INITIALIZE                    DEN-REC
              MOVE      ZERO           TO   DEN-F277
              MOVE      ZERO           TO   DEN-F274
              MOVE      PARA-SOKO      TO   DEN-F09
              MOVE      WK-TORICD(IXA) TO   DEN-F01
              MOVE      WK-SDENNO(IXA) TO   DEN-F02
              MOVE      ZERO           TO   DEN-F03
              START     SHTDENLC  KEY  >=   DEN-F277  DEN-F274
                                            DEN-F09   DEN-F01
                                            DEN-F02   DEN-F03
                  INVALID   KEY
                        CONTINUE
                  NOT INVALID
                        READ     SHTDENLC
                            AT END     CONTINUE
                            NOT AT END
                                 ADD       1    TO   RD-CNT
                                 PERFORM   HENSYU-SEC
                        END-READ
              END-START
         END-IF
     END-PERFORM.
*
     MOVE     9    TO   END-FG.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　編集処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 HENSYU-SEC             SECTION.
*
     MOVE    "HENSYU-SEC"  TO    S-NAME.
*
     IF     ( DEN-F277      =     ZERO           ) AND
            ( DEN-F274      =     ZERO           ) AND
            ( DEN-F09       =     PARA-SOKO      ) AND
            ( DEN-F01       =     WK-TORICD(IXA) )
              IF ( WK-SDENNO(IXA) <=   DEN-F02        ) AND
                 ( DEN-F02        <=   WK-EDENNO(IXA) ) AND
                 ( DEN-F051        =   40             ) AND
                 ( DEN-F274        =   0              ) AND
                 ( DEN-F04         =   0              ) AND
                 ( DEN-F134        =   0              )
*                  送信用伝票データ出力
                   MOVE     SPACE      TO   SDE-REC
                   INITIALIZE               SDE-REC
                   MOVE     DEN-REC    TO   SDE-REC
                   IF       SDE-F01  =  173
                            PERFORM  TEN-READ-SEC
                   END-IF
                   PERFORM  TANABAN-GET-SEC
                   MOVE     ZERO       TO   SDE-F56
*///DISPLAY "SDE-F56 = " SDE-F56 UPON CONS
                   WRITE    SDE-REC
                   ADD      1          TO   WRT-CNT
*
              END-IF
     ELSE
         GO        TO        HENSYU-EXIT
     END-IF.
*
 HENSYU-010.
*
     READ     SHTDENLC
         AT END    MOVE      9         TO  END-FG
         NOT AT END
                   ADD  1    TO   RD-CNT
                   GO        TO   HENSYU-SEC
     END-READ.
*
 HENSYU-EXIT.
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
     CLOSE     SHTDENLC  JHTDENF  HTENMS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*           店舗マスタ読込み                                   *
****************************************************************
 TEN-READ-SEC          SECTION.
*
     MOVE     "TEN-READ-SEC"  TO      S-NAME.
*
     MOVE     SDE-F01    TO        TEN-F52.
     MOVE     SDE-F07    TO        TEN-F011.
     READ     HTENMS     INVALID
              GO              TO   TEN-READ-EXIT
     END-READ.
*売り場コードが１の場合
     IF       TEN-F79   =  1
              MOVE   1   TO        SDE-F27F
     END-IF.
*
 TEN-READ-EXIT.
     EXIT.
****************************************************************
*           _番取得処理                                       *
****************************************************************
 TANABAN-GET-SEC       SECTION.
*
     MOVE     "TANABAN-ET-SEC"  TO    S-NAME.
*
*_番取得（商品変換テーブル）
     IF       DEN-F25   NOT   =    SPACE
              MOVE    DEN-F01         TO   SH1-F01
              MOVE    DEN-F25         TO   SH1-F02
              READ    SHOTBL1
                      INVALID   KEY
                      MOVE    SPACE   TO   SDE-F49
                      NOT INVALID    KEY
                      MOVE    SH1-F08 TO   SDE-F49
                      GO              TO   TANABAN-GET-EXIT
             END-READ
     END-IF.
*
     MOVE    DEN-F01         TO   SHO-F01.
     MOVE    DEN-F08         TO   SHO-F04
     MOVE    DEN-F1411       TO   SHO-F031
     MOVE    DEN-F1412       TO   SHO-F032
     READ    HSHOTBL
           INVALID   KEY
             MOVE    SPACE   TO   SDE-F49
           NOT INVALID    KEY
             MOVE    SHO-F08 TO   SDE-F49
     END-READ.
*
 TANABAN-GET-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
