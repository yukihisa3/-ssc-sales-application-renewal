****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　部門間在庫移動機能構築　　　      *
*    モジュール名　　　　：　ＡＣＯＳ計上ＦＬＧ戻し処理　      *
*    作成日／更新日　　　：　2018/01/18                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　パラメタにて日付を受取、振替移動　*
*                        ：　実績Ｆのパラ日付と同一の計上日デ  *
*                        ：　ータの計上ＦＬＧを初期化する。　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBZ0110B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          18/01/18.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<<振替移動実績ファイル>>----*
     SELECT   FURIDOF   ASSIGN              DA-01-VI-FURIDOL2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   IDO-F87  IDO-F05
                                                 IDO-F06  IDO-F03
                                                 IDO-F04  IDO-F01
                                                 IDO-F02  IDO-F07
                        FILE      STATUS    IS   IDO-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<振替移動実績ファイル>>----*
 FD  FURIDOF.
     COPY     FURIDOF   OF        XFDLIB
              JOINING   IDO       PREFIX.
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  WK-STATUS.
     03  IDO-ST         PIC  X(02).
*
 01  PG-ID              PIC  X(08)     VALUE  "SBZ0110B".
 01  END-FLG            PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT       PIC  9(07)     VALUE  ZERO.
     03  RWT-CNT        PIC  9(07)     VALUE  ZERO.
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
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
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC  X(01).
 01  LINK-IN-YMD6            PIC  9(06).
 01  LINK-IN-YMD8            PIC  9(08).
 01  LINK-OUT-RET            PIC  X(01).
 01  LINK-OUT-YMD            PIC  9(08).
 LINKAGE                SECTION.
 01  PARA-DATE               PIC  9(08).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-DATE.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 ACSERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      FURIDOF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SBZ0110B FURIDOL2 ERROR " IDO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN  UNTIL  END-FLG = "END".
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*クリア
     INITIALIZE    WK-CNT.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SBZ0110B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
***  日付
     MOVE     "3"                 TO        LINK-IN-KBN.
     MOVE     SYS-DATE            TO        LINK-IN-YMD6.
     MOVE     ZERO                TO        LINK-IN-YMD8.
     MOVE     ZERO                TO        LINK-OUT-RET.
     MOVE     ZERO                TO        LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING        LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD        TO        SYS-YYMD.
     DISPLAY  "DATE="   SYS-YYMD  UPON CONS.
*
     OPEN     I-O       FURIDOF.
*振替移動実績ファイルスタート
     MOVE     SPACE               TO        IDO-REC.
     INITIALIZE                             IDO-REC.
*
     MOVE     PARA-DATE           TO        IDO-F87.
     START  FURIDOF  KEY  IS  >=  IDO-F87 IDO-F05 IDO-F06 IDO-F03
                                  IDO-F04 IDO-F01 IDO-F02 IDO-F07
            INVALID
            DISPLAY  NC"＃＃対象データ存在無１＃＃" UPON CONS
            MOVE     "END"        TO        END-FLG
            GO                    TO        100-INIT-RTN-EXIT
     END-START.
*
     PERFORM  FURIDOF-READ-SEC.
*
     IF   END-FLG = "END"
          DISPLAY  NC"＃＃対象データ存在無２＃＃" UPON CONS
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*振替移動実績ファイルＡＣＯＳ計上ＦＬＧ初期化
     MOVE     SPACE          TO   IDO-F86.
     MOVE     ZERO           TO   IDO-F87  IDO-F88.
     REWRITE  IDO-REC.
     ADD      1              TO   RWT-CNT.
*
     PERFORM  FURIDOF-READ-SEC.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    FURIDOF.
*
     DISPLAY "FURIDOF READ CNT = "  READ-CNT  UPON CONS.
     DISPLAY "FURIDOF FLG  CLR = "  RWT-CNT   UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SBZ0110B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    振替移動実績ファイル読込
*--------------------------------------------------------------*
 FURIDOF-READ-SEC       SECTION.
*
     READ  FURIDOF
           AT  END   MOVE  "END"       TO   END-FLG
                     GO                TO   FURIDOF-READ-EXIT
     END-READ.
*
     ADD   1                           TO   READ-CNT.
*計上日がパラ：日付より大きくなったら終了
     IF   IDO-F87  >  PARA-DATE
          MOVE     "END"               TO   END-FLG
     END-IF.
*
 FURIDOF-READ-EXIT.
     EXIT.
