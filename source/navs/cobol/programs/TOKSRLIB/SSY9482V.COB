****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ヨドバシ　　　　　　              *
*    モジュール名　　　　：　ヨドバシ受注データ作成　　        *
*    作成日／作成者　　　：　2023/04/10 INOUE                  *
*    処理概要　　　　　　：　受注確認用データとして　　　　　　*
*    　　　　　　　　　　　　ＣＳＶ形式で出力する。　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY9482V.
*                  流用:SSY9431V
 AUTHOR.                NAV-ASSIST.
 DATE-WRITTEN.          2023/01/30.
*
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
*
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.         CONSOLE   IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<基本情報ファイル >>*************************
     SELECT   YODJOHL4           ASSIGN    TO   DA-01-VI-YODJOHL4
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    JOH-F15
                                                JOH-F16
                                                JOH-F17
                                                JOH-F28
                                                JOH-F14
                                                JOH-F18
                                                JOH-F19
                                 STATUS         JOH-STATUS.
****<<売上伝票Ｆ　　　　　 >>*********************************
     SELECT   SHTDENLJ           ASSIGN    TO   DA-01-VI-SHTDENLJ
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    DEN-F46
                                                DEN-F47
                                                DEN-F01
                                                DEN-F112
                                                DEN-F48
                                                DEN-F02
                                                DEN-F04
                                                DEN-F051
                                                DEN-F07
                                                DEN-F03
                                 STATUS         DEN-STATUS.
*****<<ＣＳＶデータ　　      >>*******************************
     SELECT   SNDJYU03          ASSIGN    TO   SNDJYU03
                                STATUS         CSV-STATUS.
*                                                              *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = 基本情報ファイル                         *
*--------------------------------------------------------------*
 FD  YODJOHL4           LABEL RECORD   IS   STANDARD.
     COPY     YODJOHL4  OF        XFDLIB
              JOINING   JOH       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 売上伝票Ｆ　　　　　　　　                       *
*--------------------------------------------------------------*
 FD  SHTDENLJ           LABEL RECORD   IS   STANDARD.
     COPY     SHTDENLJ  OF        XFDLIB
              JOINING   DEN       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ＣＳＶデータ　　　　　                   *
*--------------------------------------------------------------*
 FD  SNDJYU03           BLOCK CONTAINS 1    RECORDS.
 01  CSV-REC.
     03  FILLER         PIC       X(500).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
 01  END-FLG2                     PIC       X(03)  VALUE  SPACE.
 01  SET-FLG                      PIC       X(03)  VALUE  SPACE.
 01  SHTDENLJ-INV-FLG             PIC       X(03)  VALUE  SPACE.
 01  RD1-FLG                      PIC       X(01)  VALUE  SPACE.
 01  RD2-FLG                      PIC       X(01)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  JOH-STATUS                   PIC       X(02).
 01  DEN-STATUS                   PIC       X(02).
 01  CSV-STATUS                   PIC       X(02).
*
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD                   PIC       9(06)  VALUE  ZERO.
     03  SYS-DATEW                PIC       9(08)  VALUE  ZERO.
     03  SYS-DATE-R               REDEFINES SYS-DATEW.
         05  SYS-YY               PIC       9(04).
         05  SYS-MM               PIC       9(02).
         05  SYS-DD               PIC       9(02).
***** システム時刻ワーク
 01  SYSTEM-TIME.
     03  SYS-HH                   PIC  9(02).
     03  SYS-MN                   PIC  9(02).
     03  SYS-SS                   PIC  9(02).
***** 日付ワーク
 01  WK-HIZUKE.
     03  WK-DATE                  PIC  9(08)  VALUE  ZERO.
     03  WK-DATE-R                REDEFINES WK-DATE.
         05  WK-DATE-YYYY         PIC  9(04).
         05  WK-DATE-MM           PIC  9(02).
         05  WK-DATE-DD           PIC  9(02).
***** 時刻ワーク
 01  WK-JIKOKU.
     03  WK-TIME                  PIC  9(06)  VALUE  ZERO.
     03  WK-TIME-R                REDEFINES WK-TIME.
         05  WK-TIME-HH           PIC  9(02).
         05  WK-TIME-MM           PIC  9(02).
         05  WK-TIME-SS           PIC  9(02).
***** 時刻ワーク２
 01  WK-JIKOKU2.
     03  WK-TIME2                  PIC  9(04)  VALUE  ZERO.
     03  WK-TIME2-R                REDEFINES WK-TIME2.
         05  WK-TIME2-HH           PIC  9(02).
         05  WK-TIME2-MM           PIC  9(02).
***** 範囲チェックワーク
 01  CHK-WORK.
     03  CHK-PARA-FROM.
         05  PARA-FROM-DATE       PIC       9(08).
         05  PARA-FROM-TIME       PIC       9(06).
     03  CHK-PARA-FROM-R          REDEFINES CHK-PARA-FROM.
         05  PARA-FROM-DATETIME   PIC       9(14).
*
     03  CHK-PARA-TO.
         05  PARA-TO-DATE         PIC       9(08).
         05  PARA-TO-TIME         PIC       9(06).
     03  CHK-PARA-TO-R            REDEFINES CHK-PARA-TO.
         05  PARA-TO-DATETIME     PIC       9(14).
*
     03  CHK-DATA.
         05  DATA-DATE            PIC       9(08).
         05  DATA-TIME            PIC       9(06).
     03  CHK-DATA-R               REDEFINES CHK-DATA.
         05  DATA-DATETIME        PIC       9(14).
*
 01  WK-TENCD.
     03  WK-TENCD1                PIC       9(01)  VALUE ZERO.
     03  WK-TENCD2                PIC       X(04)  VALUE ZERO.
***** カウンタ
 01  READ-CNT                     PIC       9(07)  VALUE  ZERO.
 01  READ2-CNT                    PIC       9(07)  VALUE  ZERO.
 01  OUTPUT-CNT                   PIC       9(07)  VALUE  ZERO.
 01  IX1                          PIC       9(04)  VALUE  ZERO.
*
*明細エリア
     COPY     SNDJYU03   OF        XFDLIB
              JOINING    MEI       PREFIX.
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY9482V".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY9482V".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY9482V".
         05  FILLER               PIC       X(10)  VALUE
                       " ABEND ###".
*
     03  MSG-ABEND2.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-FL-ID            PIC       X(08).
         05  FILLER               PIC       X(04)  VALUE
                       " ST-".
         05  ERR-STCD             PIC       X(02).
         05  FILLER               PIC       X(04)  VALUE
                       " ###".
*
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " IN   = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUT  = ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-IN-BDATE       PIC   9(08).
 01  PARA-IN-BTIME       PIC   9(04).
 01  PARA-IN-BTORI       PIC   9(08).
 01  PARA-IN-SOKO        PIC   X(02).
 01  PARA-IN-NDATEF      PIC   9(08).
 01  PARA-IN-NDATET      PIC   9(08).
 01  PARA-IN-KAKTEI      PIC   X(01).
*01  PARA-OUT-CNT        PIC   9(07).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING
                                           PARA-IN-BDATE
                                           PARA-IN-BTIME
                                           PARA-IN-BTORI
                                           PARA-IN-SOKO
                                           PARA-IN-NDATEF
                                           PARA-IN-NDATET
                                           PARA-IN-KAKTEI.
*                                          PARA-OUT-CNT.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           YODJOHL4.
     MOVE     "YODJOHL4"          TO        ERR-FL-ID.
     MOVE     JOH-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE SHTDENLJ.
     MOVE     "SHTDENLJ"          TO        ERR-FL-ID.
     MOVE     DEN-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC3         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE SNDJYU03.
     MOVE     "SNDJYU03"          TO        ERR-FL-ID.
     MOVE     CSV-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY9482V-START         SECTION.
*
     MOVE   "SSY9482V-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     IF   END-FLG  NOT =  "END"
          PERFORM    MAIN-SEC  UNTIL  END-FLG   =  "END"
     END-IF.
     PERFORM            END-SEC.
     STOP               RUN.
*
 SSY9482V-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     YODJOHL4.
     OPEN     INPUT     SHTDENLJ.
     OPEN     OUTPUT    SNDJYU03.
     DISPLAY  MSG-START UPON CONS.
*
     ACCEPT   SYSYMD    FROM      DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYSYMD    TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE      LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
              MOVE    ZERO             TO   SYS-DATEW
     END-IF.
     ACCEPT    SYSTEM-TIME       FROM      TIME.
*
     MOVE  SPACE                TO   JOH-REC.
     INITIALIZE                      JOH-REC.
     MOVE  PARA-IN-BDATE        TO   JOH-F15.
     MOVE  PARA-IN-BTIME        TO   JOH-F16.
     MOVE  PARA-IN-BTORI        TO   JOH-F17.
     MOVE  PARA-IN-SOKO         TO   JOH-F28.
     MOVE  PARA-IN-NDATEF       TO   JOH-F14.
*
     START  YODJOHL4  KEY  >=   JOH-F15  JOH-F16  JOH-F17
                                JOH-F28  JOH-F14  JOH-F18
                                JOH-F19
         INVALID   KEY
            MOVE     "END"      TO   END-FLG
            DISPLAY NC"＃対象データ無し１＃" UPON CONS
            DISPLAY "JOH-F15=" JOH-F15       UPON CONS
            DISPLAY "JOH-F16=" JOH-F16       UPON CONS
            DISPLAY "JOH-F17=" JOH-F17       UPON CONS
            DISPLAY "JOH-F28=" JOH-F28       UPON CONS
            DISPLAY "JOH-F14=" JOH-F14       UPON CONS
            GO                  TO   INIT-EXIT
     END-START.
*  指示範囲保管
     MOVE   PARA-IN-NDATEF      TO   PARA-FROM-DATE.
     MOVE   PARA-IN-NDATET      TO   PARA-TO-DATE.
*
     PERFORM  YODJOHL4-RD-SEC.
     IF    END-FLG   =   "END"
           DISPLAY NC"＃対象データ無し２＃" UPON CONS
           GO                  TO   INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    基本情報ファイル読み込み　　　
****************************************************************
 YODJOHL4-RD-SEC            SECTION.
*
     MOVE    "YODJOHL4-RD-SEC"    TO   S-NAME.
*
 YODJOHL4-RD-01.
     READ     YODJOHL4
          AT END
              MOVE     "END"      TO   END-FLG
              GO                  TO   YODJOHL4-RD-EXIT
     END-READ.
*
 YODJOHL4-RD-02.
     IF       JOH-F15   NOT   =   PARA-IN-BDATE
              MOVE     "END"      TO   END-FLG
              GO                  TO   YODJOHL4-RD-EXIT
     END-IF.
*
 YODJOHL4-RD-03.
     IF       JOH-F16   NOT   =   PARA-IN-BTIME
              GO                  TO   YODJOHL4-RD-01
     END-IF.
*
 YODJOHL4-RD-04.
     IF       JOH-F17   NOT   =   PARA-IN-BTORI
              MOVE     "END"      TO   END-FLG
              GO                  TO   YODJOHL4-RD-EXIT
     END-IF.
*
 YODJOHL4-RD-05.
     IF       JOH-F28   NOT   =   PARA-IN-SOKO
              GO                  TO   YODJOHL4-RD-01
     END-IF.
*
 YODJOHL4-RD-06.
     IF     ( JOH-F14         <   PARA-IN-NDATEF )
        OR  ( JOH-F14         >   PARA-IN-NDATET )
              GO                  TO   YODJOHL4-RD-01
     END-IF.
*
 YODJOHL4-RD-07.
     IF       PARA-IN-KAKTEI  =   "1"
        IF    JOH-F36     NOT =   " "
              GO                  TO   YODJOHL4-RD-01
        END-IF
     END-IF.
     IF       PARA-IN-KAKTEI  =   "2"
        IF    JOH-F36         =   " "
              GO                  TO   YODJOHL4-RD-01
        END-IF
     END-IF.
*
     ADD   1      TO  READ-CNT.
*
 YODJOHL4-RD-EXIT.
     EXIT.
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*初期化
     MOVE     SPACE               TO   CSV-REC.
     MOVE     SPACE               TO   MEI-REC.
     INITIALIZE                        MEI-REC.
*項目転送
     MOVE   X"28"                 TO   MEI-F11S.
     MOVE   X"29"                 TO   MEI-F11E.
*
     MOVE   ","                   TO   MEI-K01
                                       MEI-K02
                                       MEI-K03
                                       MEI-K04
                                       MEI-K05
                                       MEI-K06
                                       MEI-K07
                                       MEI-K08
                                       MEI-K09
                                       MEI-K10
                                       MEI-K11
                                       MEI-K12
                                       MEI-K13
                                       MEI-K14
                                       MEI-K15
                                       MEI-K16
                                       MEI-K17
                                       MEI-K18
                                       MEI-K19
                                       MEI-K20
                                       MEI-K21
                                       MEI-K22
                                       MEI-K23.
*
*  明細行編集
*  バッチ日付
     MOVE     JOH-F15           TO   MEI-F01.
*  バッチ時刻
     MOVE     JOH-F16           TO   MEI-F02.
*  バッチ取引先
     MOVE     JOH-F17           TO   MEI-F03.
*  基幹伝票番号
     MOVE     JOH-F18           TO   MEI-F04.
*  基幹行番号
     MOVE     JOH-F19           TO   MEI-F05.
*  倉庫　　　
     MOVE     JOH-F28           TO   MEI-F06.
*  出荷先コード
     MOVE     JOH-F06           TO   MEI-F07.
*  納入期日（受注時）
     MOVE     JOH-F30           TO   WK-DATE.
     MOVE     WK-DATE-YYYY      TO   MEI-F08(1:4).
     MOVE     "/"               TO   MEI-F08(5:1).
     MOVE     WK-DATE-MM        TO   MEI-F08(6:2).
     MOVE     "/"               TO   MEI-F08(8:1).
     MOVE     WK-DATE-DD        TO   MEI-F08(9:2).
***  DISPLAY "JOH-F30 = " JOH-F30  UPON CONS.
*  納入期日（最終変更時）
     MOVE     JOH-F14           TO   WK-DATE.
     MOVE     WK-DATE-YYYY      TO   MEI-F09(1:4).
     MOVE     "/"               TO   MEI-F09(5:1).
     MOVE     WK-DATE-MM        TO   MEI-F09(6:2).
     MOVE     "/"               TO   MEI-F09(8:1).
     MOVE     WK-DATE-DD        TO   MEI-F09(9:2).
***  DISPLAY "JOH-F14 = " JOH-F14  UPON CONS.
*  品目番号
     MOVE     JOH-F08           TO   MEI-F10.
*  商品名
     MOVE     JOH-F38           TO   MEI-F11.
*  サカタ商品＋品単
     MOVE     JOH-F15           TO   DEN-F46.
     MOVE     JOH-F16           TO   DEN-F47.
     MOVE     JOH-F17           TO   DEN-F01.
     MOVE     JOH-F14           TO   DEN-F112.
     MOVE     JOH-F28           TO   DEN-F48.
     MOVE     JOH-F18           TO   DEN-F02.
     MOVE     0                 TO   DEN-F04.
     MOVE     40                TO   DEN-F051.
     MOVE     JOH-F06(1:4)      TO   WK-TENCD2.
     MOVE     WK-TENCD          TO   DEN-F07.
     MOVE     JOH-F19           TO   DEN-F03.
     PERFORM  SHTDENLJ-READ-SEC.
     IF       SHTDENLJ-INV-FLG = "INV"
              MOVE ALL "*"      TO    MEI-F12
**************在庫引当
              MOVE SPACE        TO    MEI-F15
     ELSE
              MOVE DEN-F1411    TO    MEI-F12(1:8)
              MOVE DEN-F1412    TO    MEI-F12(9:8)
**************在庫引当
              MOVE DEN-F27D     TO    MEI-F15
              IF   DEN-F15  =  ZERO
                   MOVE  "1"    TO    MEI-F15
              END-IF
              IF   MEI-F15  =  "1"
                   MOVE  SPACE  TO    MEI-F15
              ELSE
                   MOVE  "1"    TO    MEI-F15
              END-IF
     END-IF.
*  発注数量（受注時）
     MOVE     JOH-F31           TO   MEI-F13.
*  発注数量（最終変更）
     MOVE     JOH-F13           TO   MEI-F14.
*  発注先コード
     MOVE     JOH-F05           TO   MEI-F16.
*  購買発注番号
     MOVE     JOH-F02           TO   MEI-F17.
*  発注伝票日付
     MOVE     JOH-F03           TO   WK-DATE.
     MOVE     WK-DATE-YYYY      TO   MEI-F18(1:4).
     MOVE     "/"               TO   MEI-F18(5:1).
     MOVE     WK-DATE-MM        TO   MEI-F18(6:2).
     MOVE     "/"               TO   MEI-F18(8:1).
     MOVE     WK-DATE-DD        TO   MEI-F18(9:2).
*  仕入先コード
     MOVE     JOH-F04           TO   MEI-F19.
*  明細行番号
     MOVE     JOH-F07           TO   MEI-F20.
*  明細フリーテキスト１
     MOVE     JOH-F09           TO   MEI-F21.
*  お客様伝票番号
     MOVE     JOH-F11           TO   MEI-F22.
*  配送場所
     MOVE     JOH-F12           TO   MEI-F23.
*
*レコードセット
     MOVE     MEI-REC      TO   CSV-REC.
*
     WRITE    CSV-REC.
     ADD      1            TO   OUTPUT-CNT.
*    次レコード読込み
     PERFORM  YODJOHL4-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     DISPLAY "READ-CNT   = " READ-CNT    UPON  CONS.
     DISPLAY "OUTPUT-CNT = " OUTPUT-CNT  UPON  CONS.
*****MOVE     OUTPUT-CNT     TO   PARA-OUT-CNT.
*
     CLOSE    YODJOHL4  SHTDENLJ  SNDJYU03.
     DISPLAY  MSG-END   UPON  CONS.
*
 END-EXIT.
     EXIT.
***************************************************************
*             売上伝票Ｆ読込
***************************************************************
 SHTDENLJ-READ-SEC        SECTION.
*
     MOVE    "SHTDENLJ-READ-SEC"   TO        S-NAME.
*
     READ     SHTDENLJ
              INVALID      MOVE  "INV"    TO   SHTDENLJ-INV-FLG
              NOT  INVALID MOVE  SPACE    TO   SHTDENLJ-INV-FLG
     END-READ.
*
 SHTDENLJ-READ-EXIT.
     EXIT.
