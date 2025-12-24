# SKP0020L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SKP0020L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷管理システム　　　　　　　　　*
*    モジュール名　　　　：　欠品集計リスト発行　　            *
*    作成日／更新日　　　：　2011/01/28                        *
*    作成者／更新者　　　：　NAV OONO                          *
*    処理概要　　　　　　：　欠品集計Ｆを読み、欠品集計リストを*
*                            発行する。　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKP0020L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          11/01/28.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE         IS      CONS
*
     YA              IS      PITCH-2
     YB              IS      PITCH-15
     YB-21           IS      PITCH-3
     YB-22           IS      YB-22.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*欠品集計ファイル
     SELECT   KEPALLF   ASSIGN    TO        DA-01-VI-KEPALLL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KEP-F01   KEP-F02
                                            KEP-F03   KEP-F04
                                            KEP-F05
                        FILE      STATUS    KEP-STATUS.
*取引先マスタ
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE      STATUS    TOK-STATUS.
*%* プリンター *%*
     SELECT     PRTF       ASSIGN    TO        LP-04.
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*欠品集計ファイル
******************************************************************
 FD  KEPALLF            LABEL RECORD   IS   STANDARD.
     COPY     KEPALLF   OF        XFDLIB
              JOINING   KEP       PREFIX.
*
******************************************************************
*取引先マスタ
******************************************************************
 FD  HTOKMS
                        LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK  AS   PREFIX.
*
*****************************************************************
*プリンター
*****************************************************************
 FD  PRTF
     LABEL       RECORD    IS        OMITTED
     LINAGE      IS        80        LINES
     DATA        RECORD    IS        PRT-REC.
 01  PRT-REC               PIC X(200).
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*****************************************************************
*
*ステータス領域
 01  WK-ST.
     03  KEP-STATUS          PIC  X(02).
     03  TOK-STATUS          PIC  X(02).
     03  PRT-STATUS          PIC  X(02).
*フラグ領域
 01  WK-FLG.
     03  END-FLG             PIC  X(03)     VALUE  SPACE.
     03  HTOKMS-INV-FLG      PIC  X(03)     VALUE  SPACE.
*カウント領域
 01  WK-CNT.
     03  READ-CNT            PIC  9(07)     VALUE  ZERO.
     03  L-CNT               PIC  9(02)     VALUE  ZERO.
     03  P-CNT               PIC  9(03)     VALUE  ZERO.
     03  WK-KEP-F04          PIC  9(08)     VALUE  ZERO.
*取引先名称退避
 01  WK-TOKNM                PIC  N(15)     VALUE  SPACE.
*システム日付編集領域
 01  WK-AREA.
     03  SYS-DATE            PIC  9(06).
     03  SYS-DATEW           PIC  9(08).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
*
*データ退避
 01  WK-SYSBI.
     03   WK-SYS-YY      PIC  9(04).
     03   FILLER         PIC  X(01)    VALUE   "/".
     03   WK-SYS-MM      PIC  9(02).
     03   FILLER         PIC  X(01)    VALUE   "/".
     03   WK-SYS-DD      PIC  9(02).
*
 01  WK-NOUDT.
     03   WK-NOU-YY      PIC  9(04).
     03   FILLER         PIC  X(01)    VALUE   "/".
     03   WK-NOU-MM      PIC  9(02).
     03   FILLER         PIC  X(01)    VALUE   "/".
     03   WK-NOU-DD      PIC  9(02).
*
 01  WK-HATDT.
     03   WK-HAT-YY      PIC  9(04).
     03   FILLER         PIC  X(01)    VALUE   "/".
     03   WK-HAT-MM      PIC  9(02).
     03   FILLER         PIC  X(01)    VALUE   "/".
     03   WK-HAT-DD      PIC  9(02).
*メッセージ領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SKP0020L".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  END-PG          PIC   X(08)  VALUE "SKP0020L".
         05  FILLER          PIC   X(11)  VALUE
                                          " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  END-PG          PIC   X(08)  VALUE "SKP0020L".
         05  FILLER          PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  AB-FILE         PIC   X(08).
         05  FILLER          PIC   X(06)  VALUE " ST = ".
         05  AB-STS          PIC   X(02).
         05  FILLER          PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  FILLER          PIC   X(07)  VALUE " SEC = ".
         05  S-NAME          PIC   X(30).
     03  MSG-IN.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  FILLER          PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT          PIC   9(06).
         05  FILLER          PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  FILLER          PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT         PIC   9(06).
         05  FILLER          PIC   X(05)  VALUE " *** ".
*
  01  LINK-AREA.
      03  LINK-IN-KBN        PIC   X(01).
      03  LINK-IN-YMD6       PIC   9(06).
      03  LINK-IN-YMD8       PIC   9(08).
      03  LINK-OUT-RET       PIC   X(01).
      03  LINK-OUT-YMD8      PIC   9(08).
*************************
*    ﾐﾀﾞｼ  1  ｷﾞｮｳﾒ     *
*************************
*
 01  HEAD01.
     02  FILLER      PIC X(29)   VALUE  SPACE.
     02  FILLER      PIC N(10)   VALUE
         NC"＜　未納品リスト　＞" CHARACTER   TYPE   YB-22.
*
 01  HEAD02          CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(01)   VALUE  SPACE.
     02  TOKUCD      PIC 9(08).
     02  FILLER      PIC X(01)   VALUE  SPACE.
     02  TOKUNM      PIC N(15)   CHARACTER   TYPE   PITCH-2.
     02  FILLER      PIC X(25)   VALUE  SPACE.
     02  SDATE       PIC 9(10).
     02  FILLER      PIC X(01)   VALUE  SPACE.
     02  STIME       PIC 9(05).
     02  FILLER      PIC X(03)   VALUE  SPACE.
     02  FILLER      PIC N(01)   VALUE  NC"頁".
     02  FILLER      PIC X(01)   VALUE  ":".
     02  TPAGE       PIC ZZZ9.
*
 01  HEAD03          CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(01)  VALUE  SPACE.
     02  FILLER      PIC N(04)  VALUE  NC"納品日：".
     02  NOUDT       PIC X(10).
*
 01  SEN1.
     02  FILLER      PIC X(92)  VALUE  ALL  "=".
*
 01  SEN2.
     02  FILLER      PIC X(92)  VALUE  ALL  "-".
*
 01  HEAD04          CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(02)  VALUE  SPACE.
     02  FILLER      PIC N(04)  VALUE  NC"商品ＣＤ".
     02  FILLER      PIC X(07)  VALUE  SPACE.
     02  FILLER      PIC N(03)  VALUE  NC"商品名".
     02  FILLER      PIC X(32)  VALUE  SPACE.
     02  FILLER      PIC N(03)  VALUE  NC"発注数".
     02  FILLER      PIC X(08)  VALUE  SPACE.
     02  FILLER      PIC N(03)  VALUE  NC"出荷数".
     02  FILLER      PIC X(08)  VALUE  SPACE.
     02  FILLER      PIC N(03)  VALUE  NC"欠品数".
*
*************************
*    ﾒｲｻｲ     ｷﾞｮｳ      *
*************************
 01  MEISAI01.
     02  FILLER      PIC X(02)  VALUE  SPACE.
     02  MEIJANCD    PIC X(13).
     02  FILLER      PIC X(02)  VALUE  SPACE.
     02  MEISYONM    PIC X(30).
     02  FILLER      PIC X(02)  VALUE  SPACE.
     02  MEIHATYU    PIC -,---,--9.99.
     02  FILLER      PIC X(02)  VALUE  SPACE.
     02  MEISYUKA    PIC -,---,--9.99.
     02  FILLER      PIC X(02)  VALUE  SPACE.
     02  MEIKEPPN    PIC -,---,--9.99.
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE                           DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KEPALLF.
     MOVE      "KEPALLL1"   TO   AB-FILE.
     MOVE      KEP-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTOKMS.
     MOVE      "TOKMS1 "    TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   PRTF.
     MOVE      "PRTF "      TO   AB-FILE.
     MOVE      PRT-STATUS   TO   AB-STS.
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
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
     STOP  RUN.
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
     OPEN     INPUT     KEPALLF  HTOKMS.
     OPEN     OUTPUT    PRTF.
*
     MOVE     99           TO        L-CNT.
*開始メッセージ出力
     DISPLAY  MSG-START UPON CONS.
*システム日付編集（システム日付により、日付を８桁に変換）
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
     MOVE      LINK-OUT-YMD8(1:4) TO   WK-SYS-YY.
     MOVE      LINK-OUT-YMD8(5:2) TO   WK-SYS-MM.
     MOVE      LINK-OUT-YMD8(7:2) TO   WK-SYS-DD.
*
     MOVE      WK-SYSBI           TO   SDATE.
*
*画面表示時刻編集
     ACCEPT    SYS-TIME           FROM TIME.
     MOVE      SYS-TIME(1:2)      TO   HEN-TIME-HH.
     MOVE      SYS-TIME(3:2)      TO   HEN-TIME-MM.
*
     MOVE      HEN-TIME           TO   STIME.
*
*欠品集計ファイル読込
     PERFORM KEPALLF-READ-SEC.
*終了チェック
     IF      END-FLG = "END"
             MOVE  "4010"    TO   PROGRAM-STATUS
             STOP RUN
     ELSE
             MOVE  KEP-F04   TO   WK-KEP-F04
             MOVE  KEP-F03   TO   TOK-F01
             PERFORM HTOKMS-READ-SEC
             IF    HTOKMS-INV-FLG = "INV"
                   MOVE ALL NC"＊"   TO  WK-TOKNM
             ELSE
                   MOVE TOK-F02      TO  WK-TOKNM
             END-IF
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*行カウントが７０以上で、納品日がブレイクした場合
     IF   L-CNT  >  70
          PERFORM HEAD-SEC
          MOVE   KEP-F04  TO  WK-KEP-F04
     END-IF.
*納品日ブレイク
     IF   KEP-F04  NOT =  WK-KEP-F04
          PERFORM HEAD-SEC
          MOVE   KEP-F04  TO  WK-KEP-F04
     END-IF.
*明細転送
*　JANCD
     MOVE    KEP-F05      TO  MEIJANCD.
*　商品名
     MOVE    KEP-F06      TO  MEISYONM.
*　発注数
     MOVE    KEP-F07      TO  MEIHATYU.
*　出荷数
     MOVE    KEP-F08      TO  MEISYUKA.
*　欠品数
     MOVE    KEP-F09      TO  MEIKEPPN.
*　明細行印字
     WRITE     PRT-REC   FROM MEISAI01 AFTER     1.
     WRITE     PRT-REC   FROM SEN2     AFTER     1.
*　行カウント
     ADD     2            TO  L-CNT.
*欠品集計ファイル読込
     PERFORM KEPALLF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*ヘッダ行印字
****************************************************************
 HEAD-SEC              SECTION.
*
     MOVE     "HEAD-SEC"           TO  S-NAME.
*改頁
     IF   P-CNT > ZERO
          MOVE   SPACE   TO   PRT-REC
          WRITE  PRT-REC   AFTER PAGE
     END-IF.
*頁カウントアップ後、セット
     ADD      1                   TO   P-CNT.
     MOVE     P-CNT               TO   TPAGE.
*取引先ＣＤ／取引先名セット
     MOVE     KEP-F03             TO   TOKUCD.
     MOVE     WK-TOKNM            TO   TOKUNM.
*納品日
     MOVE     KEP-F04(1:4)        TO   WK-NOU-YY.
     MOVE     KEP-F04(5:2)        TO   WK-NOU-MM.
     MOVE     KEP-F04(7:2)        TO   WK-NOU-DD.
*
     MOVE     WK-NOUDT            TO   NOUDT.
*ヘッダ出力
     WRITE     PRT-REC   FROM HEAD01   AFTER     2.
     WRITE     PRT-REC   FROM HEAD02   AFTER     1.
     WRITE     PRT-REC   FROM HEAD03   AFTER     2.
     WRITE     PRT-REC   FROM SEN1     AFTER     1.
     WRITE     PRT-REC   FROM HEAD04   AFTER     1.
     WRITE     PRT-REC   FROM SEN1     AFTER     1.
*行カウント
     MOVE      8                  TO   L-CNT.
*
 HEAD-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "出力頁数  = " P-CNT     UPON CONS.
*
     CLOSE     KEPALLF  HTOKMS  PRTF.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　欠品集計ファイル読込
****************************************************************
 KEPALLF-READ-SEC    SECTION.
*
     READ     KEPALLF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  KEPALLF-READ-EXIT
     END-READ.
*欠品数が０の場合は対象外とする。
     IF      KEP-F09  =  ZERO
             GO                TO  KEPALLF-READ-SEC
     END-IF.
*
     ADD      1                TO  READ-CNT.
*
 KEPALLF-READ-EXIT.
     EXIT.
****************************************************************
*　　取引先マスタ読込
****************************************************************
 HTOKMS-READ-SEC     SECTION.
*
     READ     HTOKMS
              INVALID      MOVE "INV"   TO  HTOKMS-INV-FLG
              NOT  INVALID MOVE SPACE   TO  HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
