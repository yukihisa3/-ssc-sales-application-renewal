****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　未請求データ削除　　　　　　　　　*
*    作成日／更新日　　　：　93/10/13                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　請求合計ファイルから請求を　　　　*
*                        ：　行わないデータを削除する　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSE0080B.
 AUTHOR.                TOMI.
 DATE-WRITTEN.          93/10/13.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 請求合計ファイル >>--*
     SELECT   HSEIGKF   ASSIGN         DA-01-VI-SEIGKF21
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SEI-F01   SEI-F05
                        STATUS         SEI-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 請求合計ファイル >>--*
 FD  HSEIGKF            LABEL RECORD   IS   STANDARD.
     COPY     SETGKFA   OF        XFDLIB
              JOINING   SEI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01)  VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SEI-ST             PIC  X(02).
*
 01  COUNTER.
     03  READ-CNT       PIC  9(06)  VALUE  ZERO.
     03  DEL-CNT        PIC  9(06)  VALUE  ZERO.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 請求合計ファイル >>--*
 HSEIGKF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSEIGKF.
     DISPLAY  "### SSE0080B HSEIGKF ERROR " SEI-ST " "
                                           " ###"
                                       UPON CONS.
     MOVE     "4000"   TO    PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*                  ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 PROG-CNTL              SECTION.
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN  UNTIL     END-FLG   =    1.
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 INIT-RTN               SECTION.
*
     DISPLAY  "*** SSE0080B START *** "     UPON CONS.
*
     OPEN     I-O       HSEIGKF.
*
     READ     HSEIGKF
        AT    END
              MOVE      1    TO   END-FLG
        NOT   AT END
              ADD       1    TO   READ-CNT
     END-READ.
*
 INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 MAIN-RTN               SECTION.
*請求フラグが”９”　または　
*伝票_の上２桁が”０７”かつ請求金額が　_－_　の場合削除
     IF       SEI-F08   =    9         OR
            ((SEI-F06   <    ZERO )    AND
             (SEI-F05(1:2)   NOT  =    "07") )
*
              DELETE    HSEIGKF
              ADD       1    TO   DEL-CNT
     END-IF.
*
     READ     HSEIGKF
        AT    END
              MOVE      1    TO   END-FLG
        NOT   AT END
              ADD       1    TO   READ-CNT
     END-READ.
*
*--------------------------------------------------------------*
*                  ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 END-RTN                SECTION.
*
     CLOSE    HSEIGKF.
     DISPLAY "+++ ｾｲｷｭｳ IN ="    READ-CNT " +++" UPON CONS.
     DISPLAY "+++ ｾｲｷｭｳ DEL="    DEL-CNT  " +++" UPON CONS.
     DISPLAY  "*** SSE0080B END   *** "     UPON CONS.
 END-RTN-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
