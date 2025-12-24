# SSY7530B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY7530B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭサンワ　　　　　　　　　　　*
*    モジュール名　　　　：　伝票くんデータ抽出　　　　　　　　*
*    作成日／更新日　　　：　2016/01/08                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　パラメタを受取り、基本情報Ｆより  *
*                        ：　伝票くんデータを作成する。　　　　*
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY7530B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          16/01/08.
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
                                            DEN-F07   DEN-F112
                                            DEN-F03
                        FILE      STATUS    DEN-STATUS.
*ＤＣＭサンワ基本情報ファイル
     SELECT   SWJOHOF   ASSIGN    TO        DA-01-VI-SWJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JOH-F01   JOH-F02
                                            JOH-F03   JOH-F04
                                            JOH-F06   JOH-F05
                                            JOH-F07   JOH-F08
                        FILE      STATUS    JOH-STATUS.
*ＤＣＭサンワ伝票くん用ファイル
     SELECT   DENPKUNF  ASSIGN    TO        DA-01-S-DENPKUNF
                        FILE      STATUS    DPK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENF            LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    ＤＣＭサンワ基本情報ファイル
******************************************************************
 FD  SWJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     SWJOHOF   OF        XFDLIB
              JOINING   JOH       PREFIX.
******************************************************************
*    ＤＣＭサンワ伝票くん用ファイル
******************************************************************
 FD  DENPKUNF           BLOCK     CONTAINS  10   RECORDS.
     COPY     DENPKUNF  OF        XFDLIB
              JOINING   DPK       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  DEN-CNT1            PIC  9(08)     VALUE  ZERO.
     03  DEN-CNT2            PIC  9(08)     VALUE  ZERO.
     03  KOS-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
*
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  JOH-STATUS        PIC  X(02).
     03  DPK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY7530B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7530B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7530B".
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
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME1            PIC  9(06)  VALUE  ZERO.
     03  WK-TIME2            PIC  9(02)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS               PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y            PIC  9(02)  VALUE  ZERO.
         05  WK-M            PIC  9(02)  VALUE  ZERO.
         05  WK-D            PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE            PIC  9(08).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD       PIC   9(08).
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
                        PROCEDURE   SWJOHOF.
     MOVE      "SWJOHOL1"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DENPKUNF.
     MOVE      "DENPKUNF"   TO   AB-FILE.
     MOVE      DPK-STATUS   TO   AB-STS.
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
     OPEN     I-O       SWJOHOF.
     OPEN     INPUT     SHTDENF.
     OPEN     OUTPUT    DENPKUNF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG.
*
*----<< システム日付取得 >>--*
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*
*----<< システム時刻取得 >>--*
     ACCEPT    TIME-AREA        FROM   TIME.
*    ＤＣＭサンワ基本情報保存データスタート
     MOVE     SPACE          TO   JOH-REC.
     INITIALIZE                   JOH-REC.
     MOVE     PARA-JDATE     TO   JOH-F01.
     MOVE     PARA-JTIME     TO   JOH-F02.
     MOVE     PARA-TORICD    TO   JOH-F03.
     MOVE     PARA-SOKO      TO   JOH-F04.
     MOVE     PARA-NOUDT     TO   JOH-F06.
     START    SWJOHOF   KEY  >=   JOH-F01   JOH-F02
                                  JOH-F03   JOH-F04
                                  JOH-F06   JOH-F05
                                  JOH-F07   JOH-F08
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    ＤＣＭサンワ基本情報保存データ読込み
     PERFORM SWJOHOF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 SWJOHOF-READ-SEC    SECTION.
*
     READ     SWJOHOF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  SWJOHOF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  JOH-F01
     AND      PARA-JTIME  =  JOH-F02
     AND      PARA-TORICD =  JOH-F03
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  SWJOHOF-READ-EXIT
     END-IF.
*    取引先のチェック
     IF       PARA-TORICD  =  ZERO
              CONTINUE
     ELSE
              IF   PARA-TORICD  =  JOH-F03
                   CONTINUE
              ELSE
                   MOVE     "END"        TO  END-FLG
                   GO                    TO  SWJOHOF-READ-EXIT
              END-IF
     END-IF.
*    倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
              IF   PARA-SOKO  =  JOH-F04
                   CONTINUE
              ELSE
                   GO            TO  SWJOHOF-READ-SEC
              END-IF
     END-IF.
*    納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF  PARA-NOUDT  =  JOH-F06
                  CONTINUE
              ELSE
                  GO        TO   SWJOHOF-READ-SEC
              END-IF
     END-IF.
*
 SWJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    売上伝票ファイル検索
     MOVE     JOH-F03             TO   DEN-F01.
     MOVE     JOH-F07             TO   DEN-F02.
     MOVE     0                   TO   DEN-F04.
     MOVE     "40"                TO   DEN-F051.
     MOVE     JOH-F08             TO   DEN-F03.
     MOVE     JOH-F05             TO   DEN-F07.  *> 店舗CD
     MOVE     JOH-F06             TO   DEN-F112. *> 納品日
     READ     SHTDENF    INVALID
              MOVE    "INV"       TO   SHTDENF-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   SHTDENF-INV-FLG
     END-READ.
*
     MOVE     SPACE               TO   DPK-REC.
     INITIALIZE                        DPK-REC.
*
     MOVE   SYS-DATE              TO   DPK-F01.
     MOVE   JOH-F203              TO   DPK-F02.
     MOVE   JOH-F205              TO   DPK-F03.
     MOVE   JOH-F206              TO   DPK-F04.
     MOVE   JOH-F305              TO   DPK-F05.
     MOVE   JOH-F208              TO   DPK-F06.
     MOVE   JOH-F209              TO   DPK-F07.
     MOVE   JOH-F210              TO   DPK-F08.
     MOVE   JOH-F211              TO   DPK-F09.
     MOVE   JOH-F212              TO   DPK-F10.
     MOVE   JOH-F213              TO   DPK-F11.
     MOVE   JOH-F214              TO   DPK-F12.
     MOVE   JOH-F215              TO   DPK-F13.
     MOVE   JOH-F216              TO   DPK-F14.
     MOVE   JOH-F220              TO   DPK-F15.
     MOVE   JOH-F221              TO   DPK-F16.
     MOVE   X"28"                 TO   DPK-A011.
     MOVE   JOH-F504              TO   DPK-F17.
     MOVE   JOH-F505              TO   DPK-F18.
     MOVE   JOH-F506              TO   DPK-F19.
     MOVE   X"29"                 TO   DPK-A012.
     MOVE   JOH-F310              TO   DPK-F20.
     MOVE   JOH-F308              TO   DPK-F21.
     MOVE   JOH-F309              TO   DPK-F22.
     MOVE   JOH-F311              TO   DPK-F23.
     MOVE   JOH-F09               TO   DPK-F24.
     MOVE   JOH-F312              TO   DPK-F25.
     MOVE   JOH-F313              TO   DPK-F26.
     MOVE   JOH-F700              TO   DPK-F27.
     MOVE   JOH-F701              TO   DPK-F28.
     MOVE   X"28"                 TO   DPK-A021.
     MOVE   JOH-F608              TO   DPK-F29.
     MOVE   JOH-F609              TO   DPK-F30.
     MOVE   X"29"                 TO   DPK-A022.
     MOVE   JOH-F404              TO   DPK-F31.
     MOVE   JOH-F405              TO   DPK-F32.
     MOVE   SYS-DATE              TO   DPK-F33(1:8).
     MOVE   WK-TIME1              TO   DPK-F33(9:6).
*
     IF   SHTDENF-INV-FLG  =  SPACE
          COMPUTE JOH-F09  =  DEN-F15  *  10
          MOVE    "1"             TO   JOH-F10
          MOVE    JOH-F09         TO   DPK-F24
          REWRITE JOH-REC
          ADD     1               TO   DEN-CNT1
     ELSE
          IF  JOH-F10  =  "1"
              MOVE  JOH-F09       TO   DPK-F24
          ELSE
              MOVE  JOH-F311      TO   DPK-F24
          END-IF
          ADD     1               TO   DEN-CNT2
     END-IF.
*
     WRITE DPK-REC.
     ADD  1                       TO   KOS-CNT.
 MAIN010.
*    ＤＣＭサンワ基本情報保存データ読込み
     PERFORM SWJOHOF-READ-SEC.
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
     DISPLAY "SWJOHOF         READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "SHTDENF NOT INVALID  CNT = " DEN-CNT1  UPON CONS.
     DISPLAY "SHTDENF INVALID      CNT = " DEN-CNT2  UPON CONS.
     DISPLAY "DENPKUNF       WRITE CNT = " KOS-CNT   UPON CONS.
*
     CLOSE     SHTDENF  SWJOHOF  DENPKUNF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
