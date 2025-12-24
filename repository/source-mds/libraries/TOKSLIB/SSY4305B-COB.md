# SSY4305B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY4305B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　アークランドサカモトオンライン　　*
*    モジュール名　　　　：　欠品案内配信データ作成　　　　　　*
*    作成日／更新日　　　：　01/01/13                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　欠品案内配信データを作成する。    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY4305B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          02/01/13.
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
*----<< 欠品案内ファイル >>-*
     SELECT   ARKKEPF   ASSIGN         DA-01-VI-ARKKEPL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  ARK-F07
                                       ARK-F04
                                       ARK-F05
                        STATUS         ARK-ST.
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
*----<< 欠品案内ファイル >>-*
 FD  ARKKEPF            LABEL RECORD   IS   STANDARD.
     COPY     ARKKEPF   OF        XFDLIB
              JOINING   ARK       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 配信データ >>--*
 FD  CVCSF              LABEL     RECORD   IS   STANDARD.
 01  ONL-REC.
     03  ONL1-REC               OCCURS    2.
       05  ONL-F01              PIC  X(128).
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)    VALUE  SPACE.
     03  IDX            PIC  9(01)    VALUE  ZERO.
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
     03  GYO-CNT        PIC  9(07).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  ARK-ST             PIC  X(02).
 01  TOK-ST             PIC  X(02).
 01  CVCS-ST            PIC  X(02).
*
*----<< ｼｭｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  KEI-KEN        PIC  9(05)     VALUE  ZERO.
     03  KEI-KIN        PIC S9(09)     VALUE  ZERO.
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-AREA.
     03  WK-TORICD      PIC  9(06).
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
 01  WK-HEAD.
     03  WK-HEAD01      PIC  X(02).
     03  WK-HEAD02      PIC  X(01).
     03  WK-HEAD03      PIC  9(08).
     03  FILLER         PIC  X(117).
*
 01  WK-BODY.
     03  WK-BODY01      PIC  X(02).
     03  WK-BODY02      PIC  X(01).
     03  WK-BODY03      PIC  9(04).
     03  WK-BODY04      PIC  9(07).
     03  WK-BODY05      PIC  9(08).
     03  WK-BODY06      PIC  9(02).
     03  WK-BODY07      PIC  9(06)V9.
     03  FILLER         PIC  X(97).
*
 01  WK-TAIL.
     03  WK-TAIL01      PIC  X(02).
     03  WK-TAIL02      PIC  X(01).
     03  WK-TAIL03      PIC  9(06).
     03  FILLER         PIC  X(119).
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
         05  FILLER     PIC  N(04)  VALUE  NC"配信件数".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(09).
         05  ME-03      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-04      PIC  N(16).
         05  FILLER     PIC  X(02).
         05  ME-05      PIC  Z,ZZZ,ZZ9.
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
*日付変換用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-KENSU-FLG         PIC   9(01).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-KENSU-FLG.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ワークファイル >>--*
 ARKKEPF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ARKKEPF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY4305B ARKKEPF ERROR " ARK-ST  " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY4305B HTOKMS ERROR " TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 配信データ >>--*
 CVCSKHAI-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      CVCSF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY4305B CVCSF    ERROR " CVCS-ST " "
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
     PERFORM  MAIN-RTN  UNTIL  END-FLG  =  "END".
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
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     DISPLAY  "*** SSY4305B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*ファイルＯＰＥＮ
     OPEN     I-O       ARKKEPF.
     OPEN     INPUT     HTOKMS.
     OPEN     OUTPUT    CVCSF.
     OPEN     OUTPUT    PRTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
     INITIALIZE         GOKEI-AREA.
     INITIALIZE         WK-AREA.
*CVCS-FILE INITIALIZE
     MOVE     SPACE                    TO   ONL-REC.
     INITIALIZE                             ONL-REC.
*欠品案内ファイル読み込み
     PERFORM  ARKKEPF-READ-SEC.
     IF       END-FLG  NOT =  "END"
              MOVE      SPACE          TO   WK-HEAD
              INITIALIZE                    WK-HEAD
              MOVE      1              TO   IDX
              MOVE      "30"           TO   WK-HEAD01
              MOVE      "A"            TO   WK-HEAD02
              MOVE      LINK-OUT-YMD8  TO   WK-HEAD03
              MOVE      WK-HEAD        TO   ONL-F01(IDX)
              MOVE      1              TO   GYO-CNT
     END-IF.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*
     IF       IDX       =    2
              WRITE     ONL-REC
              ADD       1         TO   OUT-CNT
              MOVE      ZERO      TO   IDX
              MOVE      SPACE     TO   ONL-REC
              INITIALIZE               ONL-REC
     END-IF.
     ADD      1              TO   IDX.
*欠品案内ファイル情報セット
     MOVE     SPACE          TO   WK-BODY.
     INITIALIZE                   WK-BODY.
     MOVE     "30"           TO   WK-BODY01.
     MOVE     "D"            TO   WK-BODY02.
     MOVE     0001           TO   WK-BODY03.
     MOVE     116501         TO   WK-BODY04.
     MOVE     ARK-F04        TO   WK-BODY05.
     MOVE     ARK-F05        TO   WK-BODY06.
     MOVE     ARK-F06        TO   WK-BODY07.
     MOVE     WK-BODY        TO   ONL-F01(IDX).
     ADD      1              TO   GYO-CNT.
*欠品案内ファイル送信済更新
     MOVE     1              TO   ARK-F07.
     MOVE     LINK-OUT-YMD8  TO   ARK-F08.
     REWRITE  ARK-REC.
*
     PERFORM  ARKKEPF-READ-SEC.
*
 MAIN-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　終了処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     IF       OUT-CNT  >  ZERO
     OR       GYO-CNT  >  ZERO
               MOVE       1       TO   PARA-KENSU-FLG
               IF       IDX       =    2
                        WRITE     ONL-REC
                        ADD       1         TO   OUT-CNT
                        MOVE      ZERO      TO   IDX
                        MOVE      SPACE     TO   ONL-REC
                        INITIALIZE               ONL-REC
               END-IF
*テイル出力
               ADD      1                   TO   GYO-CNT
*行カウンターセット
               ADD      1                   TO   IDX
*テイル行セット
               MOVE     SPACE               TO   WK-TAIL
               INITIALIZE                        WK-TAIL
               ADD       1                  TO   OUT-CNT
               MOVE     "30"                TO   WK-TAIL01
               MOVE     "T"                 TO   WK-TAIL02
               MOVE     OUT-CNT             TO   WK-TAIL03
               MOVE     WK-TAIL             TO   ONL-F01(IDX)
               WRITE    ONL-REC
*
     ELSE
               MOVE       0       TO   PARA-KENSU-FLG
     END-IF.
*---<< HEAD  ｲﾝｻﾂ >>---*
     PERFORM  HEAD-PRINT.
*---<< ｺﾞｳｹｲ ｲﾝｻﾂ >>---*
     PERFORM  PRINT-RTN.
*
     CLOSE    ARKKEPF.
     CLOSE    HTOKMS.
     CLOSE    CVCSF.
     CLOSE    PRTF.
*
     DISPLAY "+++ ｹｯﾋﾟﾝﾃﾞｰﾀ INPUT =" IN-CNT  " +++" UPON CONS.
     DISPLAY "+++ ﾊｲｼﾝ ﾃﾞｰﾀ OUTPUT=" OUT-CNT " +++" UPON CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY4305B END *** "
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
     MOVE     116501         TO   ME-03.
     MOVE     116501         TO   TOK-F01.
     PERFORM  TOK-READ.
     MOVE     TOK-F03        TO   ME-04.
     MOVE     GYO-CNT        TO   ME-05.
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
*　　　　欠品案内ファイルＲＥＡＤ　　　　　　　　　　　　　　　*
****************************************************************
 ARKKEPF-READ-SEC            SECTION.
     READ     ARKKEPF   AT   END
              MOVE      "END"         TO   END-FLG
              GO   TO   ARKKEPF-READ-EXIT
     END-READ.
*
     IF       ARK-F07   =   "1"
              MOVE      "END"         TO   END-FLG
              GO   TO   ARKKEPF-READ-EXIT
     END-IF.
*
     ADD      1              TO   IN-CNT.
*
 ARKKEPF-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
