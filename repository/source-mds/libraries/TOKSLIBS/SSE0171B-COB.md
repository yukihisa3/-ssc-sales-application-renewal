# SSE0171B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSE0171B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ジョイフル山新オンライン　　　　　*
*    モジュール名　　　　：　請求データ作成　　　　　　　　　　*
*    作成日／更新日　　　：　00/08/02                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　請求合計ファイルより　　　　　　　*
*　　　　　　　　　　　　：　送信用ファイルを作成する          *
*    2001/01/09 伝票番号桁変更　６から７桁へ(TAKAHASHI)        *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSE0171B.
 AUTHOR.                Y.YOSHIDA.
 DATE-WRITTEN.          00/08/02.
 DATE-COMPILED.
 SECURITY.              NONE.
*
 ENVIRONMENT            DIVISION.
*
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 請求合計ファイル>>-*
     SELECT   SEIGKF    ASSIGN         DA-01-VI-HSEIGKF
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SEI-F01
                                       SEI-F03
                                       SEI-F04
                                       SEI-F05
                        STATUS         SEI-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         TOK-ST.
*----<< 配信データ >>--*
     SELECT   CVCSF     ASSIGN         DA-01-S-CVCSF
                        ORGANIZATION   SEQUENTIAL
                        STATUS    CVCS-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 請求合計ファイル>>-*
 FD  SEIGKF             LABEL RECORD   IS   STANDARD.
     COPY     SETGKFA   OF        XFDLIB
              JOINING   SEI       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 配信データ >>--*
 FD  CVCSF              LABEL     RECORD    IS   STANDARD
     BLOCK              CONTAINS       4    RECORDS.
 01  ONL-REC.
     03  ONL1-REC.
         05  ONL1-A1              PIC  9(01).
         05  ONL1-A2              PIC  9(06).
         05  ONL1-A3              PIC  9(02).
*2001/01/09 ﾚｲｱｳﾄ変更 START *
*********05  ONL1-A4              PIC  9(06).
         05  ONL1-A4              PIC  9(07).
         05  ONL1-A5              PIC  9(02).
         05  ONL1-A6              PIC  9(09).
         05  ONL1-A7              PIC  9(05).
         05  ONL1-A8              PIC  9(06).
*********05  FILLER               PIC  X(27).
         05  FILLER               PIC  X(26).
*2001/01/09 ﾚｲｱｳﾄ変更 END   *
*
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01)    VALUE  ZERO.
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  TOK-ST             PIC  X(02).
 01  SEI-ST             PIC  X(02).
 01  CVCS-ST            PIC  X(02).
*
*----<< ｼｭｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  TEN-KEN        PIC  9(05)     VALUE  ZERO.
     03  TEN-KIN        PIC S9(09)     VALUE  ZERO.
     03  KEI-KEN        PIC  9(05)     VALUE  ZERO.
     03  KEI-KIN        PIC S9(09)     VALUE  ZERO.
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-AREA.
     03  WK-TORICD      PIC  9(06).
     03  WK-NENYMD.
       05  WK-NENGETU   PIC  9(06).
       05  WK-HI        PIC  9(02).
*
*----<< KEY ｴﾘｱ >>--*
 01  KEY-AREA.
     03  CUR-KEY        PIC  9(02).
     03  BRK-KEY        PIC  9(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(47)  VALUE  SPACE.
         05  FILLER     PIC  N(12)  VALUE
                        NC"【　配信データリスト　】".
         05  FILLER     PIC  X(27)  VALUE  SPACE.
         05  FILLER     PIC  X(05)  VALUE  "DATE:".
         05  HD-011     PIC  Z9.
         05  FILLER     PIC  X(01)  VALUE  "/".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  X(01)  VALUE  "/".
         05  HD-013     PIC  Z9.
 01  HEAD02.
         05  FILLER     PIC  X(110) VALUE  SPACE.
         05  FILLER     PIC  X(05)  VALUE  "PAGE:".
         05  HD-02      PIC  ZZZ9.
 01  HEAD03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(10)  VALUE  SPACE.
         05  FILLER     PIC  N(03)  VALUE  NC"配信先".
         05  FILLER     PIC  X(35)  VALUE  SPACE.
         05  FILLER     PIC  N(04)  VALUE  NC"伝票枚数".
         05  FILLER     PIC  X(08)  VALUE  SPACE.
         05  FILLER     PIC  N(04)  VALUE  NC"請求金額".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(09).
         05  ME-03      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-04      PIC  N(16).
         05  FILLER     PIC  X(02).
         05  ME-05      PIC  ZZZ,ZZ9.
         05  FILLER     PIC  X(02).
         05  ME-06      PIC  --,---,---,--9.
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN        PIC X(01).
 01  LINK-IN-YMD6       PIC 9(06).
 01  LINK-IN-YMD8       PIC 9(08).
 01  LINK-OUT-RET       PIC X(01).
 01  LINK-OUT-YMD       PIC 9(08).
 LINKAGE                SECTION.
 01  PARA-OUT-CNT       PIC 9(07).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-OUT-CNT.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ワークファイル >>--*
 SEIGKF-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SEIGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSE0171B SEIGKF ERROR " SEI-ST  " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSE0171B HTOKMS ERROR " TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 配信データ >>--*
 CVCSKHAI-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      CVCSF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSE0171B CVCSF    ERROR " CVCS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 GENERAL-PROCESS        SECTION.
*
     PERFORM  INIT-RTN.
     IF       END-FLG   =    0
              PERFORM       MAIN-RTN
                            UNTIL END-FLG = 1
     END-IF.
     PERFORM  END-RTN.
     STOP RUN.
 GENERAL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE0171B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     SEIGKF.
     OPEN     INPUT     HTOKMS.
     OPEN     OUTPUT    CVCSF.
     OPEN     OUTPUT    PRTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
     INITIALIZE         GOKEI-AREA.
     INITIALIZE         WK-AREA.
     INITIALIZE         KEY-AREA.
     MOVE     99             TO   LINE-CNT.
*
     MOVE     SPACE          TO   SEI-REC.
     INITIALIZE                   SEI-REC.
     MOVE     1041           TO   SEI-F01.
     START    SEIGKF    KEY  >=   SEI-F01
                                  SEI-F03
                                  SEI-F04
                                  SEI-F05
         INVALID   KEY
              MOVE      1         TO   END-FLG
              GO   TO   INIT-RTN-EXIT
     END-START.
*
     PERFORM  SEI-READ.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*
     IF       CUR-KEY   NOT =     BRK-KEY
*             店合計データ出力
              PERFORM   TEN-EDIT-RTN
              MOVE      ZERO      TO   TEN-KIN
              MOVE      ZERO      TO   TEN-KEN
              MOVE      CUR-KEY   TO   BRK-KEY
     END-IF.
*    伝票データ出力
     MOVE     SPACE          TO   ONL1-REC.
     INITIALIZE                   ONL1-REC.
     MOVE     0              TO   ONL1-A1.
     MOVE     SEI-F01        TO   ONL1-A2.
     MOVE     SEI-F03        TO   ONL1-A3.
*2001/01/09 ﾚｲｱｳﾄ変更 START *
*****MOVE     SEI-F05(4:6)   TO   ONL1-A4.
     MOVE     SEI-F05(3:7)   TO   ONL1-A4.
*2001/01/09 ﾚｲｱｳﾄ変更 END   *
     IF       SEI-F07        =    40
              MOVE      10        TO   ONL1-A5
              MOVE      SEI-F06   TO   ONL1-A6
     ELSE
              MOVE      20   TO   ONL1-A5
              COMPUTE   ONL1-A6   =    SEI-F06  *  -1
     END-IF.
     MOVE     ZERO           TO   ONL1-A7.
     MOVE     SEI-F02(1:6)   TO   ONL1-A8.
*
     MOVE    "3"             TO   LINK-IN-KBN.
     MOVE     SEI-F02        TO   LINK-IN-YMD6.
     MOVE     ZERO           TO   LINK-IN-YMD8.
     MOVE     ZERO           TO   LINK-OUT-RET.
     MOVE     ZERO           TO   LINK-OUT-YMD.
     CALL    "SKYDTCKB"   USING   LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD   TO   WK-NENYMD.
     MOVE     WK-NENGETU     TO   ONL1-A8.
*
     WRITE    ONL-REC.
     ADD      1              TO   OUT-CNT.
*    合計計算
     ADD      SEI-F06   TO   TEN-KIN   KEI-KIN.
     ADD      1         TO   TEN-KEN   KEI-KEN.
*
     PERFORM  SEI-READ.
*
     IF       END-FLG   =    1
         IF   TEN-KEN   NOT  =    ZERO
*             店合計データ出力
              PERFORM   TEN-EDIT-RTN
         END-IF
*
         IF   KEI-KEN   NOT  =    ZERO
*             全店合計データ出力
              MOVE      SPACE          TO   ONL1-REC
              INITIALIZE                    ONL1-REC
              MOVE      9              TO   ONL1-A1
              MOVE      WK-TORICD      TO   ONL1-A2
              MOVE      ZERO           TO   ONL1-A3
              MOVE      ZERO           TO   ONL1-A4
              MOVE      ZERO           TO   ONL1-A5
              MOVE      KEI-KIN        TO   ONL1-A6
              MOVE      KEI-KEN        TO   ONL1-A7
              MOVE      WK-NENGETU     TO   ONL1-A8
              WRITE     ONL-REC
              ADD       1         TO   OUT-CNT
         END-IF
     END-IF.
*
 MAIN-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　店合計データ出力　　　　　　　　　　　　　　　　　　　*
****************************************************************
 TEN-EDIT-RTN           SECTION.
*
     MOVE     SPACE          TO   ONL1-REC.
     INITIALIZE                   ONL1-REC.
     MOVE     8              TO   ONL1-A1.
     MOVE     WK-TORICD      TO   ONL1-A2.
     MOVE     BRK-KEY        TO   ONL1-A3.
     MOVE     ZERO           TO   ONL1-A4.
     MOVE     ZERO           TO   ONL1-A5.
     MOVE     TEN-KIN        TO   ONL1-A6.
     MOVE     TEN-KEN        TO   ONL1-A7.
     MOVE     WK-NENGETU     TO   ONL1-A8.
     WRITE    ONL-REC.
     ADD      1              TO   OUT-CNT.
*
 TEN-EDIT-EXIT.
     EXIT.
****************************************************************
*　　　　終了処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
*---<< ｺﾞｳｹｲ ｲﾝｻﾂ >>---*
     PERFORM  PRINT-RTN.
*
     CLOSE    SEIGKF.
     CLOSE    HTOKMS.
     CLOSE    CVCSF.
     CLOSE    PRTF.
*
     DISPLAY "+++ ｺﾞｳｹｲﾃﾞｰﾀ INPUT =" IN-CNT  " +++" UPON CONS.
     DISPLAY "+++ ｾｲｷｭｳﾃﾞｰﾀ OUTPUT=" OUT-CNT " +++" UPON CONS.
*##2008/08/28 内部統制対応　NAV
     MOVE     OUT-CNT        TO      PARA-OUT-CNT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE0171B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS        UPON CONS.
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　印刷処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 PRINT-RTN              SECTION.
     IF       LINE-CNT  >    63
              PERFORM   HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
     MOVE     1041           TO   ME-03.
     MOVE     1041           TO   TOK-F01.
     PERFORM  TOK-READ.
     MOVE     TOK-F03        TO   ME-04.
     MOVE     KEI-KEN        TO   ME-05.
     MOVE     KEI-KIN        TO   ME-06.
*
     WRITE    PRT-REC   FROM MEIS01    AFTER     1.
     ADD      1         TO   LINE-CNT.
 PRINT-EXIT.
     EXIT.
****************************************************************
*　　　　タイトルプリント　　　　　　　　　　　　　　　　　　　*
****************************************************************
 HEAD-PRINT             SECTION.
     IF       PAGE-CNT  >    0
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
              MOVE      ZERO      TO   LINE-CNT
     END-IF.
*
     ADD      1              TO   PAGE-CNT.
     MOVE     SYS-YY         TO   HD-011.
     MOVE     SYS-MM         TO   HD-012.
     MOVE     SYS-DD         TO   HD-013.
     MOVE     PAGE-CNT       TO   HD-02.
*
     WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     2.
     WRITE    PRT-REC   FROM      P-SPACE   AFTER     1.
     MOVE     7              TO   LINE-CNT.
 HEAD-PRINT-EXIT.
     EXIT.
****************************************************************
*　　　　取引先マスタＲＥＡＤ　　　　　　　　　　　　　　　　　*
****************************************************************
 TOK-READ               SECTION.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F03
     END-READ.
 TOK-READ-EXIT.
     EXIT.
****************************************************************
*　　　　請求合計ＦＲＥＡＤ　　　　　　　　　　　　　　　　　　*
****************************************************************
 SEI-READ               SECTION.
     READ     SEIGKF    AT   END
              MOVE      1         TO   END-FLG
              GO   TO   SEI-READ-EXIT
     END-READ.
*
     IF       SEI-F01   NOT =     1041
              MOVE      1         TO   END-FLG
              GO   TO   SEI-READ-EXIT
     END-IF.
*
     ADD      1              TO   IN-CNT.
     MOVE     SEI-F03        TO   CUR-KEY.
     IF       IN-CNT    =    1
              MOVE      SEI-F03        TO   BRK-KEY
              MOVE      SEI-F01        TO   WK-TORICD
     END-IF.
*
 SEI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
