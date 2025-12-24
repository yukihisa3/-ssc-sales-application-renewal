****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　受注自動欠品　　　　　　　　　　　*
*    モジュール名　　　　：　自動欠品結果ファイル削除　　　　　*
*    作成日／更新日　　　：　2018/07/05                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    ＩＮＰＵＴ　　　　　：　取引先コード PARA-TORICD          *
*                        ：　受信日       PARA-DATE            *
*    処理概要　　　　　　：　パラメタを受け取り、削除基準日以　*
*                        ：　下のデータを削除する。　　　　　  *
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJK9020B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/10/09.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 自動欠品結果ファイル >>--*
     SELECT   JIDKEKF   ASSIGN         DA-01-VI-JIDKEKLA
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  JID-F46
                                       JID-F47
                                       JID-F01
                                       JID-F48
                                       JID-F02
                                       JID-F04
                                       JID-F051
                                       JID-F07
                                       JID-F112
                                       JID-F03
                        STATUS         JIDKEKF-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 自動欠品結果ファイル >>--*
 FD  JIDKEKF            LABEL     RECORD   IS   STANDARD.
     COPY     JIDKEKF   OF        XFDLIB
              JOINING   JID       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  JIDKEKF-ST        PIC  X(02).
*
*----<< INVALID FLG >>--*
 01  SYU-INVALID-FLG   PIC  X(01).
 01  JID-INVALID-FLG   PIC  X(01).
*
*----<< FLAG >>--*
 01  TAISHO-FLG        PIC  X(01).
*
*----<< ｶｳﾝﾄ >>--*
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  DEL-CNT                 PIC  9(08)     VALUE  ZERO.
*
*----<< ﾒｯｾｰｼﾞ ｴﾘｱ >>--*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJK9020B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJK9020B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJK9020B".
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
         05  FILLER         PIC   X(09)  VALUE " DELETE= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 LINKAGE                SECTION.
 01  PARA-DATE              PIC   9(08).
****************************************************************
 PROCEDURE              DIVISION USING  PARA-DATE.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 自動欠品結果ファイル >>--*
 JIDKEKF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      JIDKEKF.
     MOVE      "JIDKEKF"    TO   AB-FILE.
     MOVE      JIDKEKF-ST   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
 END     DECLARATIVES.
****************************************************************
*                                                              *
****************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
*
*----<< ｼｮｷﾁ ｾｯﾄ >--*
     MOVE     "INIT-SEC"          TO   S-NAME.
*
*----<< ｼｮｷﾒｯｾｰｼﾞ ｼｭﾂﾘｮｸ >>--*
     DISPLAY  MSG-START UPON CONS.
*
*----<<FILE OPEN >>--*
     OPEN     I-O       JIDKEKF.
*
*----<< ﾍﾝｽｳ ｸﾘｱ >--*
     MOVE     ZERO      TO        END-FG    RD-CNT    DEL-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
*----<< １レコード目読込 >>--*
     PERFORM  900-KEK-READ.

 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
*自動欠品結果ファイル削除
     DELETE  JIDKEKF.
     ADD     1                    TO   DEL-CNT.
*
*----<< 次レコード読込 >>--*
     PERFORM  900-KEK-READ.

 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      DEL-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
*----<<FILE CLOSE >>--*
     CLOSE     JIDKEKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    売上伝票ファイル READ (SEQUENTIAL)           *
*--------------------------------------------------------------*
 900-KEK-READ           SECTION.
     MOVE     "900-KEK-READ"      TO   S-NAME.
     READ     JIDKEKF
       AT END
           MOVE      9    TO   END-FG
           GO  TO  900-KEK-READ-EXIT
       NOT AT END
           ADD       1    TO   RD-CNT
     END-READ.
*読み込みカウント表示
     IF   RD-CNT(6:3)  =  "000" OR "500"
          DISPLAY "RD-CNT = " RD-CNT UPON CONS
     END-IF.
*受信日チェック
     IF  JID-F46  <= PARA-DATE
         CONTINUE
     ELSE
           MOVE      9    TO   END-FG
           GO  TO  900-KEK-READ-EXIT
     END-IF.
*
 900-KEK-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >----------------------------------*
