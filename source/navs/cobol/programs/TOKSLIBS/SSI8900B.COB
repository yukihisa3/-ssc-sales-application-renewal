****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　リックデータ変換　　　　　　　　　*
*    作成日／更新日　　　：　2008/04/15                        *
*    作成者／更新者　　　：　松野　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　リックにて受信したデータ　　　　　*
*                            をＪＣＡフォーマットに変換する。　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSI8900B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/04/15.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*支払受信データファイル
     SELECT   RCSIHASF  ASSIGN    TO        DA-01-S-RCSIHASF
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    EDI-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*支払明細ファイル
     SELECT   SKTSIHF   ASSIGN    TO        DA-01-VI-SKTSIHL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SIH-F11   SIH-F01
                                            SIH-F02   SIH-F03
                                            SIH-F04
                        FILE      STATUS    SIH-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　1130 　  ＢＦ＝　3
******************************************************************
 FD  RCSIHASF
                        BLOCK CONTAINS      3    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  EDI-REC.
     03  EDI-01                  PIC  X(02).
     03  EDI-02                  PIC  X(1128).
******************************************************************
*    支払データ
******************************************************************
 FD  SKTSIHF
                        LABEL RECORD   IS   STANDARD.
     COPY     SKTSIHF   OF        XFDLIB
              JOINING   SIH       PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*ヘッダ情報格納領域
     COPY   RCSITHED OF XFDLIB  JOINING   HED  AS   PREFIX.
*明細情報格納領域
     COPY   RCSITMEI OF XFDLIB  JOINING   MEI  AS   PREFIX.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  SIH-CNT                 PIC  9(08)     VALUE  ZERO.
 01  IX                      PIC  9(02)     VALUE  ZERO.
 01  JCA-CNT4                PIC  9(08)     VALUE  ZERO.
 01  TOKMS2-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  JHMRUTL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  WK-TOK-F81              PIC  X(02)     VALUE  SPACE.
 01  WK-JOH-F17              PIC  9(02)     VALUE  ZERO.
 01  WK-DENNO.
   03  WK-DENNO1             PIC  X(03)     VALUE  SPACE.
   03  WK-DENNO2             PIC  X(06)     VALUE  SPACE.
 01  WK-TEN                  PIC  X(04).
 01  WK-TEN-R     REDEFINES  WK-TEN.
   03  WK-TENCD              PIC  9(04).
 01  WK-DENKIN               PIC  X(12).
 01  WK-GOKEI                PIC  X(12).
 01  WK-GOKEI-R   REDEFINES  WK-GOKEI.
*  03  FILERR                PIC  X(01).
   03  GOKEIKIN              PIC  S9(11).
*01  IX                      PIC  9(02).
 01  WK-DENKIN-AREA.
     03  WK-DENKIN-H         PIC  X(12).
     03  WK-DENKIN-F-R       REDEFINES    WK-DENKIN-H.
         05  WK-DENKIN-HEN   PIC  9(12).
     03  WK-DENKIN-FLG       PIC  9(01)   VALUE  ZERO.
     03  WK-DENKIN-OK        PIC S9(11)   VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  SIH-STATUS         PIC   X(02).
     03  EDI-STATUS         PIC   X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSI8900B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSI8900B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSI8900B".
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
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
 LINKAGE                SECTION.
 01  PARA-KBN               PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-KBN.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SKTSIHF.
     MOVE      "SKTSIHL1 "   TO   AB-FILE.
     MOVE      EDI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   RCSIHASF.
     MOVE      "RCSIHASF"    TO   AB-FILE.
     MOVE      SIH-STATUS    TO   AB-STS.
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
              UNTIL     END-FLG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       RCSIHASF.
     OPEN     OUTPUT    SKTSIHF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        RD-CNT.
     MOVE     ZERO      TO        SIH-CNT.
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
*
*
     PERFORM  RCSIHASF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
*    伝票ヘッダレコード
     IF    EDI-01  =   "HD"
           PERFORM  MEISAI-HEAD-SEC
     END-IF.
*
     IF    EDI-01  =   "DT"
           PERFORM  MEISAI-SIHARAI-SEC
     END-IF.
*
     PERFORM RCSIHASF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 RCSIHASF-READ-SEC      SECTION.
*
     MOVE "RCSIHASF-READ-SEC" TO       S-NAME.
*
     READ     RCSIHASF
              AT END
              MOVE     9      TO    END-FLG
              NOT AT END
              ADD      1      TO    RD-CNT
     END-READ.
*
     IF   RD-CNT(6:3) = "000" OR "500"
          DISPLAY "READ-CNT = " RD-CNT   UPON CONS
     END-IF.
*
 RCSIHASF-READ-EXIT.
     EXIT.
****************************************************************
*　　支払ヘッダレコード作成
****************************************************************
 MEISAI-HEAD-SEC    SECTION.
*
***********ワークエリア初期化
     MOVE      SPACE       TO   SIH-REC  HED-REC.
     INITIALIZE                 SIH-REC  HED-REC.
*****ヘッダ情報→ワークにセット
     MOVE      EDI-REC     TO   HED-REC.
*****支払ヘッダ作成
*    支払月
     MOVE      HED-F02(1:6)    TO   SIH-F02.
*    買掛締期間開始
     MOVE      HED-F02     TO   SIH-F12.
*    買掛締期間終了
     MOVE      HED-F03     TO   SIH-F13.
*
 MEISAI-HEAD-EXIT.
     EXIT.
****************************************************************
*　　支払明細レコード作成
****************************************************************
 MEISAI-SIHARAI-SEC SECTION.
***********ワークエリア初期化
     MOVE      SPACE       TO   MEI-REC.
     INITIALIZE                 MEI-REC.
     MOVE      SPACE       TO   END-FLG.
*****ヘッダ情報→ワークにセット
     MOVE      EDI-REC     TO   MEI-REC.
*****支払ヘッダ作成
*    取引先ＣＤ
     MOVE      MEI-F07     TO   SIH-F01.
*    店舗ＣＤ
     MOVE      MEI-F06     TO   WK-TEN.
     MOVE      WK-TENCD    TO   SIH-F03.
*    伝票日付
     MOVE      MEI-F05     TO   SIH-F04.
*    伝票区分
     MOVE      MEI-F09     TO   SIH-F05.
*    部門
     MOVE      MEI-F08     TO   SIH-F06.
*    伝票番号
     MOVE      MEI-F02     TO   SIH-F07.
*    伝票合計金額
     IF        MEI-F10  = "1"
               MOVE      SPACE       TO   SIH-F09
               MOVE      SPACE       TO   SIH-F10
               MOVE      MEI-F03     TO   WK-DENKIN
     ELSE
               MOVE      MEI-F04     TO   WK-DENKIN
     END-IF.
     MOVE       ZERO                 TO   WK-DENKIN-FLG.
     MOVE       ZERO                 TO   WK-DENKIN-OK.
     MOVE      "000000000000"        TO   WK-DENKIN-H.
*****PERFORM  VARYING  IX  FROM 12 BY -1  UNTIL END-FLG = "END"
*                                         OR    IX < 1
*             IF    WK-DENKIN(IX:1) = "-"
*                   COMPUTE  SIH-F08 = GOKEIKIN * (-1)
*                   MOVE  "END"      TO   END-FLG
*             ELSE
*                   MOVE  WK-DENKIN(IX:1) TO  WK-GOKEI(IX:1)
*                   MOVE  GOKEIKIN   TO   SIH-F08
*             END-IF
*****END-PERFORM.
     PERFORM  VARYING  IX  FROM 12 BY -1  UNTIL END-FLG = "END"
                                          OR    IX < 1
              IF    WK-DENKIN(IX:1) = "-"
                    MOVE   1         TO   WK-DENKIN-FLG
                    MOVE  "END"      TO   END-FLG
              ELSE
                    MOVE  WK-DENKIN(IX:1) TO  WK-DENKIN-H(IX:1)
              END-IF
     END-PERFORM.
*
     IF       WK-DENKIN-FLG =  ZERO
              MOVE    WK-DENKIN-HEN TO    SIH-F08
     ELSE
              COMPUTE WK-DENKIN-OK = WK-DENKIN-HEN * (-1)
              MOVE    WK-DENKIN-OK  TO    SIH-F08
     END-IF.
*    相殺コード
     IF        MEI-F10  = "2"
               MOVE      MEI-F11     TO   SIH-F09
************ 相殺名称
               MOVE      MEI-F12     TO   SIH-F10
     END-IF.
*    支払区分
     MOVE      MEI-F10     TO   SIH-F11.
*
     WRITE  SIH-REC.
     ADD    1              TO   SIH-CNT.
 MEISAI-SIHARAI-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     CLOSE     SKTSIHF   RCSIHASF.
*
     DISPLAY NC"＃ＲＥＡＤ　ＣＮＴ＝"  RD-CNT    UPON CONS.
     DISPLAY NC"＃ＯＵＴ　　ＣＮＴ＝"  SIH-CNT   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
