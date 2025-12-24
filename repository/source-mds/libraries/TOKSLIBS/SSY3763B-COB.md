# SSY3763B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3763B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ　新ＥＤＩシステム　　　　　*
*    業務名　　　　　　　：　ナフコ　新ＥＤＩシステム　　　　　*
*    モジュール名　　　　：　手書出荷指示データ削除　　　　　　*
*    作成日／更新日　　　：　2010/10/06                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取ったパラメタに該当する出荷　*
*                            情報データを削除する。　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3763B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/06/17.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
**基本情報ファイル**
     SELECT   NFJOHOF   ASSIGN    TO        DA-01-VI-NFJOHOL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       NFJ-F02   NFJ-F03
                                            NFJ-F04   NFJ-F05
                                            NFJ-F06   NFJ-F07
                                            NFJ-F08   NFJ-F09
                        FILE      STATUS    NFJ-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    基本情報ファイル
******************************************************************
 FD  NFJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     NFJOHOF   OF        XFDLIB
              JOINING   NFJ       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  DEL-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  NFJOHOF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  NFJ-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3763B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3763B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3763B".
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
 01  PARA-SAKUBA            PIC   X(02).
 01  PARA-NOUDT             PIC   9(08).
 01  PARA-SDENNO            PIC   9(09).
 01  PARA-EDENNO            PIC   9(09).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-SAKUBA
                                       PARA-NOUDT
                                       PARA-SDENNO
                                       PARA-EDENNO.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFJOHOF.
     MOVE      "NFJOHOF "   TO   AB-FILE.
     MOVE      NFJ-STATUS   TO   AB-STS.
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
     OPEN     I-O       NFJOHOF.
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
*****基本情報ファイルスタート
     MOVE     SPACE          TO   NFJ-REC.
     INITIALIZE                   NFJ-REC.
     MOVE     99999999       TO   NFJ-F02.
     MOVE     9999           TO   NFJ-F03.
*****MOVE     137607         TO   NFJ-F04.
     MOVE     999977         TO   NFJ-F04.
     MOVE     PARA-SAKUBA    TO   NFJ-F05.
     MOVE     PARA-NOUDT     TO   NFJ-F09.
     MOVE     PARA-SDENNO    TO   NFJ-F07.
     START    NFJOHOF   KEY  >=   NFJ-F02   NFJ-F03  NFJ-F04
                                  NFJ-F05   NFJ-F06  NFJ-F07
                                  NFJ-F08   NFJ-F09
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO      TO     INIT-EXIT
     END-START.
**** 基本情報ファイル読込み
     PERFORM  NFJOHOF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 NFJOHOF-READ-SEC    SECTION.
*
 NFJOHOF-READ-010.
*
     READ     NFJOHOF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  NFJOHOF-READ-EXIT
     END-READ.
 NFJOHOF-READ-020.
*    作場ＣＤチェック
     IF       PARA-SAKUBA NOT =  NFJ-F05
              GO                    TO  NFJOHOF-READ-010
     END-IF.
 NFJOHOF-READ-030.
*    伝票番号範囲チェック
     IF    (( PARA-SDENNO  >    NFJ-F07 )    OR
            ( PARA-EDENNO  <    NFJ-F07 ))
              GO                    TO  NFJOHOF-READ-010.
 NFJOHOF-READ-040.
*    納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF  PARA-NOUDT  =  NFJ-F09
                  CONTINUE
              ELSE
                  GO        TO   NFJOHOF-READ-SEC
              END-IF
     END-IF.
 NFJOHOF-READ-050.
*    確定データ読み飛ばし
     IF       NFJ-F26       =   "1"
              GO                  TO   NFJOHOF-READ-SEC.
*
      ADD     1                   TO   READ-CNT.
*
 NFJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*****基本情報ファイル削除
     DELETE   NFJOHOF.
     ADD      1                   TO   DEL-CNT.
 MAIN010.
*****基本情報ファイル読込み
     PERFORM NFJOHOF-READ-SEC.
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
     DISPLAY "NFJOHOL2 ﾐｶｸﾃｲ DT READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "NFJOHOL2 ﾐｶｸﾃｲ DT DELE CNT = " DEL-CNT   UPON CONS.
*
     CLOSE     NFJOHOF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
