# SER0110L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SER0110L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　システム共通サブシステム　        *
*    モジュール名　　　　：　連携エラーリカバリリスト　　　　　*
*    作成日／作成者　　　：　2021/11/18 INOUE                  *
*    更新日／更新者　　　：　                                  *
*    処理概要　　　　　　：　パラメタに合致するレコードより　　*
*                            リスト発行する　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SER0110L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/11/18.
*
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
*
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.         CONSOLE   IS        CONS
                        YA    IS PITCH-20        *> 2.0ピッチ
                        YA-22 IS PITCH-22        *> 2.0ピッチ
                        YA-21 IS PITCH-20-YKBAI  *> 2.0ピッチ
                        YB    IS PITCH-15        *> 1.5ピッチ
                        YB-21 IS PITCH-15-YKBAI  *> 1.5ピッチ
                        YB-22 IS PITCH-30.       *> 3.0ピッチ
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<Ｄ３６５累積ファイル>>********************************
     SELECT  ERRRUIL5  ASSIGN    TO        DA-01-VI-ERRRUIL5
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       ERR-F01
                                           ERR-F041
                                           ERR-F042
                                           ERR-F043
                                           ERR-F044
                                           ERR-F045
                                           ERR-F046
                       FILE      STATUS    ERR-STATUS.
****<<担当者マスタ　　　　　 >>*********************************
     SELECT   TANMS1             ASSIGN    TO   DA-01-VI-TANMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TAN-F01  TAN-F02
                       FILE      STATUS         TAN-STATUS.
*
*****<<  プリント　Ｆ   >>**************************************
     SELECT   PRINTF    ASSIGN    TO        LP-04-PRTF.
*                                                                *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = 　Ｄ３６５累積ファイル　　　               *
*--------------------------------------------------------------*
 FD  ERRRUIL5           LABEL RECORD   IS   STANDARD.
     COPY     ERRRUIL5  OF        XFDLIB
              JOINING   ERR       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 担当者マスタ　　　　　　　　　                     *
*--------------------------------------------------------------*
 FD  TANMS1             LABEL RECORD   IS   STANDARD.
     COPY     TANMS1    OF        XFDLIB
              JOINING   TAN       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = プリントファイル                                   *
*--------------------------------------------------------------*
 FD  PRINTF.
 01  P-REC                        PIC       X(200).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
*01  TOKMS2-INV-FLG               PIC       X(03)  VALUE  SPACE.
*01  TENMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
*01  JYOKEN1-INV-FLG              PIC       X(03)  VALUE  SPACE.
 01  TANMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  ERR-STATUS                   PIC       X(02).
*01  ERR-STATUS                   PIC       X(02).
*01  TOK-STATUS                   PIC       X(02).
*01  TEN-STATUS                   PIC       X(02).
*01  JYO-STATUS                   PIC       X(02).
 01  TAN-STATUS                   PIC       X(02).
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
*
 01  WK-AREA.
     03  IX1                      PIC       9(02)  VALUE  ZERO.
     03  SET-FLG                  PIC       X(01)  VALUE  SPACE.
     03  RD1-FLG                  PIC       X(01)  VALUE  SPACE.
     03  RD2-FLG                  PIC       X(01)  VALUE  SPACE.
*
***** カウンタ
 01  P-CNT                        PIC       9(04)  VALUE  ZERO.
 01  L-CNT                        PIC       9(03)  VALUE  ZERO.
 01  CNT-READ                     PIC       9(06)  VALUE  ZERO.
 01  MEI-CNT                      PIC       9(05)  VALUE  ZERO.
*
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SER0110L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SER0110L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SER0110L".
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
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " PAGE =".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
***** 見出し行１
 01  HD01.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  X(08)  VALUE  "SER0110L".
     03  FILLER                PIC  X(13)  VALUE  SPACE.
     03  FILLER                PIC  N(18)
             VALUE NC"＜Ｄ３６５連携エラーリカバリリスト＞"
                               CHARACTER  TYPE  IS PITCH-22.
     03  FILLER                PIC  X(14)  VALUE  SPACE.
     03  FILLER                PIC  X(04)  VALUE  SPACE.
     03  HD01-YY               PIC  9999.
     03  FILLER                PIC  N(01)  VALUE  NC"年"
                               CHARACTER  TYPE  IS PITCH-20.
     03  HD01-MM               PIC  Z9.
     03  FILLER                PIC  N(01)  VALUE  NC"月"
                               CHARACTER  TYPE IS PITCH-20.
     03  HD01-DD               PIC  Z9.
     03  FILLER                PIC  N(01)  VALUE  NC"日"
                               CHARACTER  TYPE IS PITCH-20.
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  HD01-PCNT             PIC  ZZ9.
     03  FILLER                PIC  N(02)  VALUE  NC"頁"
                               CHARACTER  TYPE IS PITCH-20.
***** 見出し行２
 01  HD02.
     03  FILLER                PIC  X(114)  VALUE  SPACE.
     03  HD02-HH               PIC  99.
     03  FILLER                PIC  N(01)  VALUE  NC"："
                               CHARACTER  TYPE  IS PITCH-20.
     03  HD02-MN               PIC  Z9.
     03  FILLER                PIC  N(01)  VALUE  NC"："
                               CHARACTER  TYPE IS PITCH-20.
     03  HD02-SS               PIC  Z9.
***** 見出し行３
 01  HD03.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  N(05)  VALUE  NC"管理番号："
                               CHARACTER   TYPE IS   PITCH-15.
     03  HD03-KANRINO          PIC  X(08)  VALUE  SPACE.
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(06)
                               VALUE  NC"バッチ日付："
                               CHARACTER   TYPE IS   PITCH-15.
     03  HD03-BDATE            PIC  X(10)  VALUE  SPACE.
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(06)
                               VALUE  NC"バッチ時刻："
                               CHARACTER   TYPE IS   PITCH-15.
     03  HD03-BTIME            PIC  X(08)  VALUE  SPACE.
***** 見出し行４
 01  HD04.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"処理日"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(06)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"時刻"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(05)  VALUE  NC"エラー区分"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(10)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"伝票番号"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(14)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"行"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"枝"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(05)  VALUE  NC"ＪＡＮＣＤ"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(12)  VALUE  SPACE.
     03  FILLER               PIC  N(05)  VALUE  NC"実行担当者"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(12)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"実行日"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(06)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"時刻"
                              CHARACTER  TYPE IS PITCH-20.
***** 線
 01  SEN1.
     03  FILLER               PIC  X(136) VALUE  ALL "-".
*
***** 線
 01  SEN2 CHARACTER   TYPE IS   PITCH-20.
     03  FILLER                   PIC       N(01)  VALUE
         NC"＜".
     03  FILLER                   PIC       N(18)  VALUE
         NC"－－－－－－－－－－－－－－－－－　".
     03  FILLER                   PIC       N(09)  VALUE
         NC"Ｄ３６５エラー情報".
     03  FILLER                   PIC       N(18)  VALUE
         NC"　－－－－－－－－－－－－－－－－－".
     03  FILLER                   PIC       N(01)  VALUE
         NC"＞".
     03  FILLER                   PIC       N(01)  VALUE
         NC"　".
     03  FILLER                   PIC       N(01)  VALUE
         NC"＜".
     03  FILLER                   PIC       N(07)  VALUE
         NC"－－－－－－　".
     03  FILLER                   PIC       N(04)  VALUE
         NC"実行情報".
     03  FILLER                   PIC       N(07)  VALUE
         NC"　－－－－－－".
     03  FILLER                   PIC       N(01)  VALUE
         NC"＞".
***** 空白行
 01  SEN3.
     03  FILLER                   PIC       X(136)  VALUE SPACE.
*
***** 明細行
 01  MD01                         CHARACTER   TYPE IS   PITCH-15.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-SDATE               PIC       X(10).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD01-STIME               PIC       X(08).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD01-ERRKBN              PIC       X(02).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-ERRNM               PIC       N(10).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD01-DENNO               PIC       X(20).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD01-GYONO               PIC       Z9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD01-EDANO               PIC       Z9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD01-JANCD               PIC       X(20).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD01-JTANCD              PIC       X(02).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-JTANNM              PIC       N(10).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD01-JDATE               PIC       X(10).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD01-JTIME               PIC       X(08).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  LINK-IN-BUMCD          PIC   X(04).
 01  LINK-IN-TANCD          PIC   X(02).
 01  LINK-IN-ERR1           PIC   X(01).
 01  LINK-IN-ERR2           PIC   X(01).
 01  LINK-IN-ERR3           PIC   X(01).
 01  LINK-IN-ERR4           PIC   X(01).
*01  LINK-IN-SEL            PIC   X(01).
 01  LINK-IN-KANRINO        PIC   9(08).
 01  LINK-IN-BDATE          PIC   9(08).
 01  LINK-IN-BTIME          PIC   9(06).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE       DIVISION  USING     LINK-IN-BUMCD
                                     LINK-IN-TANCD
                                     LINK-IN-ERR1
                                     LINK-IN-ERR2
                                     LINK-IN-ERR3
                                     LINK-IN-ERR4
*                                    LINK-IN-SEL
                                     LINK-IN-KANRINO
                                     LINK-IN-BDATE
                                     LINK-IN-BTIME.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           ERRRUIL5.
     MOVE     "ERRRUIL5"          TO        ERR-FL-ID.
     MOVE     ERR-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE      4000               TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC5         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE TANMS1.
     MOVE     "TANMS1 "          TO        ERR-FL-ID.
     MOVE     TAN-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE      4000               TO   PROGRAM-STATUS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SER0110L-START         SECTION.
*
     MOVE   "SER0110L-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
*
     IF    END-FLG    NOT =  "END"
           PERFORM    MAIN-SEC  UNTIL     END-FLG   =  "END"
     END-IF.
*
     PERFORM            END-SEC.
*
     STOP               RUN.
*
 SER0110L-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     ERRRUIL5.
*    OPEN     INPUT     TENMS1.
*    OPEN     INPUT     TOKMS2.
*    OPEN     INPUT     JYOKEN1.
     OPEN     INPUT     TANMS1.
     OPEN     OUTPUT    PRINTF.
*
*    MOVE     ZERO           TO    WK-DENKEI.
*    INITIALIZE                    WK-DENKEI.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     99             TO    L-CNT.
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
     ACCEPT   SYSTEM-TIME       FROM      TIME.
*
     MOVE  SPACE                TO   ERR-REC.
     INITIALIZE                      ERR-REC.
     MOVE   LINK-IN-KANRINO     TO   ERR-F01.
*
     START  ERRRUIL5  KEY  >=   ERR-F01  ERR-F041 ERR-F042
                                ERR-F043 ERR-F044 ERR-F045
            INVALID   KEY
               MOVE     "END"      TO   END-FLG
               DISPLAY NC"＃対象データ無し１＃" UPON CONS
               GO                  TO   INIT-EXIT
     END-START.
*
     PERFORM  ERRRUIL5-RD-SEC.
     IF    END-FLG   =   "END"
           DISPLAY NC"＃対象データ無し２＃" UPON CONS
           GO                  TO   INIT-EXIT
     END-IF.
     PERFORM   MIDASISET-SEC.
*ブレイクキー設定
*    MOVE  ERR-F02    TO       BRK-TENCD.
*    MOVE  ERR-F04    TO       BRK-DENNO.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    エラー累積データ読み込み　　　
****************************************************************
 ERRRUIL5-RD-SEC            SECTION.
*
     MOVE    "ERRRUIL5-RD-SEC"    TO   S-NAME.
*
     READ     ERRRUIL5
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    ERRRUIL5-RD-EXIT
     END-READ.
*
*
 ERRRUIL5-RD-EXIT.
     EXIT.
****************************************************************
*             見出し編集処理                1.2                *
****************************************************************
 MIDASISET-SEC             SECTION.
*
     MOVE    "MIDASISET-SEC"              TO    S-NAME.
*システム日付・時刻セット
     MOVE     SYS-YY            TO   HD01-YY.
     MOVE     SYS-MM            TO   HD01-MM.
     MOVE     SYS-DD            TO   HD01-DD.
     MOVE     SYS-HH            TO   HD02-HH.
     MOVE     SYS-MN            TO   HD02-MN.
     MOVE     SYS-SS            TO   HD02-SS.
*管理番号
     MOVE     LINK-IN-KANRINO   TO   HD03-KANRINO.
*バッチ日付
     MOVE     LINK-IN-BDATE(1:4)     TO    HD03-BDATE(1:4).
     MOVE     "/"                    TO    HD03-BDATE(5:1).
     MOVE     LINK-IN-BDATE(5:2)     TO    HD03-BDATE(6:2).
     MOVE     "/"                    TO    HD03-BDATE(8:1).
     MOVE     LINK-IN-BDATE(7:2)     TO    HD03-BDATE(9:2).
*バッチ時刻　
     MOVE     LINK-IN-BTIME(1:2)     TO    HD03-BTIME(1:2).
     MOVE     ":"                    TO    HD03-BTIME(3:1).
     MOVE     LINK-IN-BTIME(3:2)     TO    HD03-BTIME(4:2).
     MOVE     ":"                    TO    HD03-BTIME(6:1).
     MOVE     LINK-IN-BTIME(5:2)     TO    HD03-BTIME(7:2).
*
 MIDASISET-EXIT.
     EXIT.
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO    S-NAME.
*
 MAIN-01.
*対象チェック
*　　管理番号
     IF       ERR-F01   NOT =  LINK-IN-KANRINO
              MOVE     "END"      TO   END-FLG
              GO                  TO   MAIN-EXIT
     END-IF.
 MAIN-011.
*　　リカバリ対象（1～4全" "）
     IF     ( LINK-IN-ERR1 = " " ) AND
            ( LINK-IN-ERR2 = " " ) AND
            ( LINK-IN-ERR3 = " " ) AND
            ( LINK-IN-ERR4 = " " )
              GO                  TO   MAIN-02
     END-IF.
 MAIN-012.
*　　リカバリ対象1
     IF       LINK-IN-ERR1    = "1"
              IF (  ERR-F043  = "01" ) OR
                 (  ERR-F043  = "02" )
                    GO                 TO   MAIN-02
              END-IF
     END-IF.
 MAIN-013.
*　　リカバリ対象2
     IF       LINK-IN-ERR2    = "1"
              IF    ERR-F043  = "03"
                    GO                 TO   MAIN-02
              END-IF
     END-IF.
 MAIN-014.
*　　リカバリ対象3
     IF       LINK-IN-ERR3    = "1"
              IF    ERR-F043  = "04"
                    GO                 TO   MAIN-02
              END-IF
     END-IF.
 MAIN-015.
*　　リカバリ対象4
     IF       LINK-IN-ERR4    = "1"
              IF    ERR-F043  = "05"
                    GO                 TO   MAIN-02
              END-IF
     END-IF.
 MAIN-016.
*　　対象外　読み飛ばし
     GO                         TO   MAIN-99.
*
 MAIN-02.
*明細行編集出力
*  処理日
     MOVE     ERR-F041(1:4)     TO    MD01-SDATE(1:4).
     MOVE     "/"               TO    MD01-SDATE(5:1).
     MOVE     ERR-F041(5:2)     TO    MD01-SDATE(6:2).
     MOVE     "/"               TO    MD01-SDATE(8:1).
     MOVE     ERR-F041(7:2)     TO    MD01-SDATE(9:2).
*  時刻　
     MOVE     ERR-F042(1:2)     TO    MD01-STIME(1:2).
     MOVE     ":"               TO    MD01-STIME(3:1).
     MOVE     ERR-F042(3:2)     TO    MD01-STIME(4:2).
     MOVE     ":"               TO    MD01-STIME(6:1).
     MOVE     ERR-F042(5:2)     TO    MD01-STIME(7:2).
*  エラー区分
     MOVE     ERR-F043          TO    MD01-ERRKBN.
     EVALUATE ERR-F043
        WHEN  "01"
              MOVE NC"受注情報"     TO    MD01-ERRNM
        WHEN  "02"
              MOVE NC"売上情報"     TO    MD01-ERRNM
        WHEN  "03"
              MOVE NC"入荷実績情報" TO    MD01-ERRNM
        WHEN  "04"
              MOVE NC"移動実績情報" TO    MD01-ERRNM
        WHEN  "05"
              MOVE NC"作業依頼情報" TO    MD01-ERRNM
        WHEN  OTHER
              MOVE ALL NC"＊"       TO    MD01-ERRNM
     END-EVALUATE.
*  伝票番号
*----MOVE     ERR-F044          TO    MD01-DENNO.
     EVALUATE ERR-F043
        WHEN  "01"
              MOVE   ERR-F044(3:18)   TO    MD01-DENNO
        WHEN  "02"
              MOVE   ERR-F044(3:18)   TO    MD01-DENNO
        WHEN  "03"
              MOVE   ERR-F044(14:7)   TO    MD01-DENNO
        WHEN  "04"
              MOVE   ERR-F044(14:7)   TO    MD01-DENNO
        WHEN  "05"
              MOVE   ERR-F044(14:7)   TO    MD01-DENNO
        WHEN  OTHER
              MOVE   ERR-F044         TO    MD01-DENNO
     END-EVALUATE.
*  行　　　　
     MOVE     ERR-F045          TO    MD01-GYONO.
*  枝番
     MOVE     ERR-F046          TO    MD01-EDANO.
*  ＪＡＮＣＤ
     MOVE     ERR-F047          TO    MD01-JANCD.
*  実行担当者
     MOVE     ERR-F97           TO    MD01-JTANCD.
*      担当者名
     MOVE     LINK-IN-BUMCD     TO    TAN-F01.
     MOVE     ERR-F97           TO    TAN-F02.
     PERFORM  TANMS1-READ-SEC.
     IF   TANMS1-INV-FLG = "INV"
          MOVE  ALL NC"＊"     TO        MD01-JTANNM
     ELSE
          MOVE  TAN-F03        TO        MD01-JTANNM
     END-IF.
*    MOVE  "1"              TO    RD1-FLG.
*  実行日
*    MOVE     ERR-F83           TO    MD01-NYURYOKUBI.
     MOVE     ERR-F98 (1:4)     TO    MD01-JDATE(1:4).
     MOVE     "/"               TO    MD01-JDATE(5:1).
     MOVE     ERR-F98 (5:2)     TO    MD01-JDATE(6:2).
     MOVE     "/"               TO    MD01-JDATE(8:1).
     MOVE     ERR-F98 (7:2)     TO    MD01-JDATE(9:2).
*  実行時刻　
     MOVE     ERR-F99 (1:2)     TO    MD01-JTIME(1:2).
     MOVE     ":"               TO    MD01-JTIME(3:1).
     MOVE     ERR-F99 (3:2)     TO    MD01-JTIME(4:2).
     MOVE     ":"               TO    MD01-JTIME(6:1).
     MOVE     ERR-F99 (5:2)     TO    MD01-JTIME(7:2).
*
 MAIN-03.
*--------------
*  明細行出力
*--------------
*  改頁チェック
     IF       L-CNT     >    54
              PERFORM  MIDASI-SEC
*             PERFORM  HED-SEC
     END-IF.
*    IF       SET-FLG   = "1"
*             PERFORM  HED-SEC
*    END-IF.
*    IF       L-CNT     >    54
*             PERFORM  MIDASI-SEC
*             PERFORM  HED-SEC
*    END-IF.
 MAIN-04.
     WRITE  P-REC  FROM  MD01  AFTER 1.
     ADD    1            TO    L-CNT.
 MAIN-99.
*    次レコード読込み
     PERFORM  ERRRUIL5-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*             見出し出力処理                1.2                *
****************************************************************
 MIDASI-SEC             SECTION.
*
     MOVE    "MIDASI-SEC"              TO    S-NAME.
*改頁
     IF       P-CNT  >  ZERO
              MOVE   SPACE   TO   P-REC
              WRITE  P-REC   AFTER PAGE
     END-IF.
*
*頁セット
     ADD      1              TO   P-CNT.
     MOVE     P-CNT          TO   HD01-PCNT.
*ヘッダー出力
     WRITE    P-REC     FROM      HD01      AFTER     2.
     WRITE    P-REC     FROM      HD02      AFTER     1.
     WRITE    P-REC     FROM      HD03      AFTER     1.
     WRITE    P-REC     FROM      SEN1      AFTER     1.
     WRITE    P-REC     FROM      SEN2      AFTER     1.
     WRITE    P-REC     FROM      HD04      AFTER     1.
     WRITE    P-REC     FROM      SEN1      AFTER     1.
*
     MOVE     8         TO        L-CNT.
*
 MIDASI-EXIT.
     EXIT.
***************************************************************
*             担当者マスタ読込
***************************************************************
 TANMS1-READ-SEC       SECTION.
*
     MOVE    "TANMS1-READ-SEC" TO        S-NAME.
*
     READ     TANMS1
              INVALID      MOVE  "INV"    TO   TANMS1-INV-FLG
              NOT  INVALID MOVE  SPACE    TO   TANMS1-INV-FLG
     END-READ.
*
 TANMS1-READ-EXIT.
     EXIT.
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     MOVE     P-CNT     TO        OUT-CNT.
     DISPLAY  MSG-OUT   UPON CONS.
     DISPLAY  MSG-END   UPON CONS.
*
     CLOSE    ERRRUIL5  TANMS1 PRINTF.
*
 END-EXIT.
     EXIT.
*

```
