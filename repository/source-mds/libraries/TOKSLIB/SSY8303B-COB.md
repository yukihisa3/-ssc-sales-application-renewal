# SSY8303B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY8303B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ホーマックＷＥＢＥＤＩシステム　　*
*    業務名　　　　　　　：　ホーマックＷＥＢＥＤＩシステム　　*
*    モジュール名　　　　：　納品予定データ作成                *
*    作成日／更新日　　　：　2006/09/15                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            を出荷情報保存データより抽出する。*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY8303B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          04/07/22.
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
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
                                            DEN-F03
                        FILE      STATUS    DEN-STATUS.
*コーナン発注情報保存データ
     SELECT   HCJOHOF   ASSIGN    TO        DA-01-VI-HCJOHOL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HCJ-K01   HCJ-K02
                                            HCJ-K03   HCJ-K04
                                            HCJ-K08   HCJ-K05
                                            HCJ-K06   HCJ-K07
                        FILE      STATUS    HCJ-STATUS.
*コーナン出荷情報修正
     SELECT   HCSYUKF   ASSIGN    TO        DA-01-VI-HCSYUKL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HCS-K01   HCS-K02
                                            HCS-K03   HCS-K04
                                            HCS-K08   HCS-K05
                                            HCS-K06   HCS-K07
                        FILE      STATUS    HCS-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENF
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    コーナン発注情報保存データ
******************************************************************
 FD  HCJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     HCJOHOF   OF        XFDLIB
              JOINING   HCJ       PREFIX.
******************************************************************
*    コーナン発注データ
******************************************************************
 FD  HCSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     HCSYUKF   OF        XFDLIB
              JOINING   HCS       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP1-CNT           PIC  9(08)     VALUE  ZERO.
     03  SKIP2-CNT           PIC  9(08)     VALUE  ZERO.
     03  KOS1-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMK2-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMS-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  HCSYUKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  HCJ-STATUS        PIC  X(02).
     03  HCS-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY8303B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8303B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8303B".
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
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUDT             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-NOUDT.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENF.
     MOVE      "SHTDENL1"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HCJOHOF.
     MOVE      "HCJOHOL3 "   TO   AB-FILE.
     MOVE      HCJ-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HCSYUKF.
     MOVE      "HCSYUKL3 "   TO   AB-FILE.
     MOVE      HCS-STATUS   TO   AB-STS.
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
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       HCJOHOF  SHTDENF.
     OPEN     I-O       HCSYUKF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG.
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
*    コーナン発注情報保存データスタート
     MOVE     SPACE          TO   HCJ-REC.
     INITIALIZE                   HCJ-REC.
     MOVE     PARA-JDATE     TO   HCJ-K01.
     MOVE     PARA-JTIME     TO   HCJ-K02.
     MOVE     PARA-TORICD    TO   HCJ-K03.
     MOVE     PARA-SOKO      TO   HCJ-K04.
     MOVE     PARA-NOUDT     TO   HCJ-K08.
     MOVE     ZERO           TO   HCJ-K05.
     MOVE     ZERO           TO   HCJ-K06.
     MOVE     ZERO           TO   HCJ-K07.
     START    HCJOHOF   KEY  >=   HCJ-K01   HCJ-K02
                                  HCJ-K03   HCJ-K04
                                  HCJ-K08   HCJ-K05
                                  HCJ-K06   HCJ-K07
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    コーナン発注情報保存データ読込み
     PERFORM HCJOHOF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 HCJOHOF-READ-SEC    SECTION.
*
     READ     HCJOHOF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  HCJOHOF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  HCJ-K01
     AND      PARA-JTIME  =  HCJ-K02
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  HCJOHOF-READ-EXIT
     END-IF.
*    取引先のチェック
     IF       PARA-TORICD  =  ZERO
              CONTINUE
     ELSE
              IF   PARA-TORICD  =  HCJ-K03
                   CONTINUE
              ELSE
                   MOVE     "END"        TO  END-FLG
                   GO                    TO  HCJOHOF-READ-EXIT
              END-IF
     END-IF.
*    送信ＦＬＧのチェック
     IF       HCJ-K11  =  "1"
              GO                 TO   HCJOHOF-READ-SEC
     END-IF.
*    倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
              IF   PARA-SOKO  =  HCJ-K04
                   CONTINUE
              ELSE
                   GO            TO  HCJOHOF-READ-SEC
              END-IF
     END-IF.
*    納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF  PARA-NOUDT  =  HCJ-K08
                  CONTINUE
              ELSE
                  GO        TO   HCJOHOF-READ-SEC
              END-IF
     END-IF.
*
 HCJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    売上伝票ファイル検索
     MOVE     HCJ-K03             TO   DEN-F01.
     MOVE     HCJ-K06             TO   DEN-F02.
     MOVE     0                   TO   DEN-F04.
     MOVE     "40"                TO   DEN-F051.
     MOVE     HCJ-K07             TO   DEN-F03.
     READ     SHTDENF    INVALID
              MOVE    "INV"       TO   SHTDENF-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   SHTDENF-INV-FLG
     END-READ.
*
     IF       SHTDENF-INV-FLG  =  "INV"
              ADD     1           TO   SKIP2-CNT
              GO                  TO   MAIN010
     ELSE
**************数量確認(発注数<>出荷数の時ﾌｧｲﾙに出力)
              PERFORM   HCSYUKF-READ-SEC
              IF    HCSYUKF-INV-FLG = "INV"
                    PERFORM   HCSYUKF-WRITE-SEC
                    MOVE   "1"       TO   HCJ-K09
                    MOVE  SYS-DATEW  TO   HCJ-K10
                    REWRITE  HCJ-REC
              END-IF
     END-IF.
 MAIN010.
*    コーナン発注情報保存データ読込み
     PERFORM HCJOHOF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　コーナン発注確定データ作成処理　　　　　　　　　　　　　　
****************************************************************
 HCSYUKF-WRITE-SEC     SECTION.
*
     MOVE     "HCSYUKF-WRITE-SEC"  TO  S-NAME.
*
     MOVE      SPACE               TO  HCS-REC.
     INITIALIZE                        HCS-REC.
     MOVE      HCJ-REC             TO  HCS-REC.
*    出荷数量
     COMPUTE   HCS-M10  =  DEN-F50  -  DEN-F15.
*****DISPLAY "DEN-F50 = " DEN-F50 UPON CONS.
*****DISPLAY "DEN-F15 = " DEN-F15 UPON CONS.
*****DISPLAY "HCS-F10 = " HCS-F10 UPON CONS.
*    欠品区分
     IF        HCS-M10  >  ZERO
               MOVE   10           TO  HCS-M20
     END-IF.
*    ダミーセット
     MOVE      "0"                 TO  HCS-A28(48:1).
*
     WRITE     HCS-REC.
     ADD       1                   TO  KOS1-CNT.
*    ADD       1                   TO  KMS-CNT.
*
 HCSYUKF-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "ｺｰﾅﾝF       READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "ｳﾘｱｹﾞDT ﾅｼ       CNT = " SKIP2-CNT UPON CONS.
     DISPLAY "ｺｰﾅﾝ ｶｸﾃｲDT WT   CNT = " KOS1-CNT  UPON CONS.
*
     CLOSE     SHTDENF  HCJOHOF  HCSYUKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 HCSYUKF-READ-SEC    SECTION.
*
     MOVE     SPACE          TO   HCS-REC.
     INITIALIZE                   HCS-REC.
     MOVE     HCJ-K01        TO   HCS-K01.
     MOVE     HCJ-K02        TO   HCS-K02.
     MOVE     HCJ-K03        TO   HCS-K03.
     MOVE     HCJ-K04        TO   HCS-K04.
     MOVE     HCJ-K08        TO   HCS-K08.
     MOVE     HCJ-K05        TO   HCS-K05.
     MOVE     HCJ-K06        TO   HCS-K06.
     MOVE     HCJ-K07        TO   HCS-K07.
     MOVE     HCJ-K08        TO   HCS-K08.
     READ     HCSYUKF    INVALID
              MOVE    "INV"       TO   HCSYUKF-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   HCSYUKF-INV-FLG
     END-READ.
*
 HCSYUKF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
