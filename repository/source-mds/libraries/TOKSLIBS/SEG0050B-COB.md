# SEG0050B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SEG0050B.COB`

## ソースコード

```cobol
****************************************************************
*    （大阪営業所－全データ抽出バージョン）                    *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　オンラインデータ更新処理　　　　　*
*    作成日／更新日　　　：　2000/03/28                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　受信したオンラインデータを読み，  *
*                            売上伝票ファイルへ更新する。      *
*　                                                            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SEG0050B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/03/28.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受信用伝票データ
     SELECT   JHSDENF   ASSIGN    TO        JHSDENF
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    IS   SDE-STATUS.
*売上伝票データ
     SELECT   SHTDENLA  ASSIGN    TO        DA-01-VI-SHTDENLA
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
************************ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F48
                                            DEN-F02   DEN-F04
                                            DEN-F051  DEN-F03
                        FILE      STATUS    IS   DEN-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    送信用売上伝票データ
******************************************************************
 FD  JHSDENF            LABEL RECORD   IS   STANDARD.
     COPY     JHSDENF   OF        XFDLIB
              JOINING   SDE       PREFIX.
*
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENLA
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG1                PIC  X(03)     VALUE  SPACE.
 01  END-FLG2                PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  DEL-CNT                 PIC  9(08)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  SDE-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SEG0050B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SEG0050B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SEG0050B".
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
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDENF.
     MOVE      "JHSDENF "   TO   AB-FILE.
     MOVE      SDE-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENLA.
     MOVE      "SHTDENLA"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
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
              UNTIL     END-FLG1   =   "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN     INPUT     JHSDENF.
     OPEN     I-O       SHTDENLA.
     DISPLAY  MSG-START UPON CONS.
*受信オンラインデータ一件目読込み
     PERFORM JHSDENF-READ-SEC.
     IF      END-FLG1 = "END"
             GO            TO        INIT-EXIT
     ELSE
*            パラメタ（渡）セット
             MOVE  SDE-F46 TO        PARA-JDATE
             MOVE  SDE-F47 TO        PARA-JTIME
             MOVE  SDE-F01 TO        PARA-TORICD
     END-IF.
*売上伝票ファイル削除スタート
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     SDE-F46        TO   DEN-F46.
     MOVE     SDE-F47        TO   DEN-F47.
     MOVE     SDE-F01        TO   DEN-F01.
     MOVE     SPACE          TO   DEN-F48.
     START    SHTDENLA  KEY  >=   DEN-F46   DEN-F47
                                  DEN-F01   DEN-F48
                                  DEN-F02   DEN-F04
                                  DEN-F051  DEN-F03
         INVALID   KEY
         DISPLAY "AAAAA" UPON CONS
              MOVE      "END"     TO    END-FLG2
              GO   TO   INIT-EXIT
     END-START.
*
 INIT-010.
*
     PERFORM SHTDENLA-READ-SEC  UNTIL  END-FLG2 = "END".
     DISPLAY "## SHTDENF DELETE CNT = " DEL-CNT " ##" UPON CONS.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*売上伝票ファイル追加
     MOVE     SPACE               TO   DEN-REC.
     INITIALIZE                        DEN-REC.
*受信オンラインデータセット
     MOVE     SDE-REC             TO   DEN-REC.
*売上伝票データ追加
     WRITE    DEN-REC.
     ADD      1                   TO   WRT-CNT.
*受信オンラインデータ読込み
     PERFORM JHSDENF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　受信オンラインデータ読込み　　　　　　　　　　　*
****************************************************************
 JHSDENF-READ-SEC   SECTION.
*
     MOVE     "JHSDENF-READ-SEC"  TO      S-NAME.
*
     READ  JHSDENF  AT  END
               MOVE    "END"      TO      END-FLG1
     END-READ.
*
 JHSDENF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　売上伝票データ削除                              *
****************************************************************
 SHTDENLA-READ-SEC  SECTION.
*
     MOVE  "SHTDENLA-READ-SEC"    TO      S-NAME.
*
     READ  SHTDENLA  NEXT  AT  END
               MOVE  "END"        TO      END-FLG2
               GO                 TO      SHTDENLA-READ-EXIT
     END-READ.
*
     IF     ( SDE-F46    =    DEN-F46 ) AND
            ( SDE-F47    =    DEN-F47 ) AND
            ( SDE-F01    =    DEN-F01 )
              DELETE  SHTDENLA
              ADD     1           TO       DEL-CNT
     ELSE
              MOVE   "END"        TO       END-FLG2
     END-IF.
*
 SHTDENLA-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     CLOSE     SHTDENLA  JHSDENF.
*
     DISPLAY "## SHTDENF WRITE  CNT = " WRT-CNT " ##" UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
